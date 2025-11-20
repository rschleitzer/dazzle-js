/**
 * DSSSL Template Loader
 * Port from: OpenJade jade/jade.cxx template loading
 *
 * Supports two formats:
 * 1. Plain Scheme (.scm files)
 * 2. XML wrapper with entity references (.dsl files)
 *
 * XML format (OpenJade-compatible):
 * <?xml version="1.0"?>
 * <!DOCTYPE style-sheet PUBLIC "-//James Clark//DTD DSSSL Style Sheet//EN" [
 *   <!ENTITY helpers SYSTEM "helpers.scm">
 *   <!ENTITY rules SYSTEM "rules.scm">
 * ]>
 * <style-sheet>
 * <style-specification>
 * &helpers;
 * &rules;
 * (element chapter ...)
 * </style-specification>
 * </style-sheet>
 */

import * as fs from 'fs';
import * as path from 'path';
import { Catalog } from './catalog.js';

/**
 * Entity declaration from DOCTYPE
 */
interface Entity {
  name: string;
  systemId?: string;  // SYSTEM identifier (file path) - optional for PUBLIC-only entities
  publicId?: string;  // PUBLIC identifier - optional
  value?: string;     // Direct text replacement value (for <!ENTITY name "value"> declarations)
}

/**
 * Source map entry - tracks which lines came from which file
 */
export interface SourceMapEntry {
  startLine: number;  // Start line in concatenated content (1-based)
  endLine: number;    // End line in concatenated content (1-based, inclusive)
  sourceFile: string; // Original source file path
  sourceLine: number; // Line offset in original file (0-based)
}

/**
 * Template loader result
 */
export interface TemplateResult {
  schemeCode: string;
  entities: Entity[];
  sourceMap: SourceMapEntry[]; // Maps concatenated lines to source files
}

/**
 * Load and process a DSSSL template
 *
 * @param templatePath Path to template file
 * @param searchPaths Additional search paths for entity resolution and catalog loading
 * @returns Scheme code ready for parsing
 */
export function loadTemplate(templatePath: string, searchPaths: string[] = []): TemplateResult {
  const content = fs.readFileSync(templatePath, 'utf-8');
  const templateDir = path.dirname(templatePath);

  // Load catalogs from search paths
  // Port from: OpenJade command-line catalog handling
  const allSearchPaths = [templateDir, ...searchPaths];
  const catalog = Catalog.loadFromSearchPaths(allSearchPaths);

  // Detect format
  if (content.trim().startsWith('<?xml') || content.trim().startsWith('<')) {
    // XML format - parse and extract
    if (process.env.DEBUG_TEMPLATE) {
      console.error(`DEBUG_TEMPLATE: Detected XML format for ${templatePath}`);
    }
    return loadXmlTemplate(content, templatePath, templateDir, searchPaths, catalog);
  } else {
    // Plain Scheme format - single file, simple source map
    const lineCount = content.split('\n').length;
    return {
      schemeCode: content,
      entities: [],
      sourceMap: [{
        startLine: 1,
        endLine: lineCount,
        sourceFile: templatePath,
        sourceLine: 0,
      }],
    };
  }
}

/**
 * Load XML-wrapped template
 */
function loadXmlTemplate(content: string, templatePath: string, baseDir: string, searchPaths: string[], catalog: Catalog): TemplateResult {
  // Parse DOCTYPE for entity declarations (including from parameter entity files)
  const { entities, paramDefMap } = parseEntities(content, baseDir, searchPaths, catalog);

  // Parse external specifications
  // Port from: DSSSL style-sheet - <external-specification id="..." document="...">
  const externalSpecs = parseExternalSpecifications(content);

  // Get the use attribute from style-specification
  const usedSpecIds = parseStyleSpecificationUse(content);

  // Resolve entity references in content
  const mainExtraction = extractStyleSpecification(content);
  let schemeCode = mainExtraction.content;
  const mainTemplateOffset = mainExtraction.lineOffset;

  // Prepend external specifications that are in the use list
  // Port from: DSSSL - external specifications reference entities by name
  const externalContents: Array<{ content: string; path: string; lineOffset: number }> = [];
  for (const specId of usedSpecIds) {
    const spec = externalSpecs.find(s => s.id === specId);
    if (spec) {
      // Look up the entity with this name to get the actual file path
      const entity = entities.find(e => e.name === spec.document);
      if (!entity) {
        // Entity not found - skip (may be PUBLIC-only entity needing catalog)
        console.warn(`Warning: External specification "${spec.id}" references undefined entity "${spec.document}" - skipping`);
        continue;
      }

      // Resolve entity path - try SYSTEM first, then PUBLIC via catalog
      let systemId = entity.systemId;
      if (!systemId && entity.publicId) {
        // Try resolving PUBLIC identifier via catalog
        if (process.env.DEBUG_CATALOG) {
          console.error(`DEBUG: Resolving PUBLIC "${entity.publicId}" for entity "${spec.document}"`);
        }
        systemId = catalog.resolve(entity.publicId) || undefined;
        if (process.env.DEBUG_CATALOG) {
          console.error(`DEBUG: Resolved to: ${systemId || '(not found)'}`);
        }
      }

      if (!systemId) {
        // Entity has neither SYSTEM path nor resolvable PUBLIC identifier
        console.warn(`Warning: External specification "${spec.id}" references entity "${spec.document}" that cannot be resolved - skipping`);
        continue;
      }

      const specPath = resolveEntityPath(systemId, baseDir, searchPaths);
      let specContent = fs.readFileSync(specPath, 'utf-8');
      let lineOffset = 0;

      if (process.env.DEBUG_CATALOG) {
        console.error(`DEBUG: Loading external spec from: ${specPath}`);
        console.error(`DEBUG: Content length: ${specContent.length} bytes`);
      }

      // Check if this external spec is itself an XML-wrapped template
      if (specContent.trim().startsWith('<?xml') || specContent.trim().startsWith('<!DOCTYPE')) {
        // Extract style-specification content from XML wrapper
        try {
          const extraction = extractStyleSpecification(specContent);
          specContent = extraction.content;
          lineOffset = extraction.lineOffset;
        } catch (e) {
          // Not a valid XML template - use as-is
        }
      }

      // Strip CDATA and marked sections
      specContent = specContent.replace(/<!\[CDATA\[([\s\S]*?)\]\]>/g, '$1');
      specContent = specContent.replace(/<!\[%[\w-]+;?\[\s*([\s\S]*?)\s*\]\]>/g, '$1');

      if (process.env.DEBUG_CATALOG) {
        console.error(`DEBUG: After processing, content length: ${specContent.length} bytes`);
        console.error(`DEBUG: First 200 chars: ${specContent.substring(0, 200)}`);
      }

      externalContents.push({ content: specContent, path: specPath, lineOffset });
    }
  }

  // Build source map starting with external specifications
  const sourceMap: SourceMapEntry[] = [];
  let currentLine = 1;
  const prependedLines: string[] = [];

  // Add external specifications first (they provide definitions used by main code)
  for (const external of externalContents) {
    const lines = external.content.split('\n');
    prependedLines.push(...lines);

    if (process.env.DEBUG_CATALOG) {
      console.error(`DEBUG: Prepending ${lines.length} lines from ${external.path}`);
    }

    // Add source map entry for this external spec
    sourceMap.push({
      startLine: currentLine,
      endLine: currentLine + lines.length - 1,
      sourceFile: external.path,
      sourceLine: external.lineOffset - 1, // Convert 1-based lineOffset to 0-based sourceLine
    });

    currentLine += lines.length;
  }

  if (process.env.DEBUG_CATALOG) {
    console.error(`DEBUG: Total prepended lines: ${prependedLines.length}`);
  }

  // Track all entity references and their positions
  const entityReplacements: Array<{ ref: string; content: string; path: string }> = [];

  if (process.env.DEBUG_TEMPLATE) {
    console.error(`DEBUG_TEMPLATE: Processing ${entities.length} entities for replacement`);
  }

  for (const entity of entities) {
    let entityContent: string;
    let entityPath: string;

    if (process.env.DEBUG_TEMPLATE) {
      console.error(`DEBUG_TEMPLATE: Processing entity ${entity.name}: value=${entity.value ? 'YES' : 'NO'}, systemId=${entity.systemId}, publicId=${entity.publicId}`);
    }

    // Check if this is a text replacement entity
    if (entity.value !== undefined) {
      // Direct text replacement entity (<!ENTITY name "value">)
      entityContent = entity.value;
      entityPath = templatePath; // Use template path for source tracking

      const entityRef = `&${entity.name};`;
      entityReplacements.push({ ref: entityRef, content: entityContent, path: entityPath });
      continue;
    }

    // File-based entity - resolve path
    let systemId = entity.systemId;
    if (!systemId && entity.publicId) {
      // Try resolving PUBLIC identifier via catalog
      systemId = catalog.resolve(entity.publicId) || undefined;
    }

    if (!systemId) {
      // Skip entities that cannot be resolved
      continue;
    }

    entityPath = resolveEntityPath(systemId, baseDir, searchPaths);
    entityContent = fs.readFileSync(entityPath, 'utf-8');

    if (process.env.DEBUG_TEMPLATE) {
      console.error(`DEBUG_TEMPLATE: Loading entity file: ${entityPath}`);
      console.error(`  Has DOCTYPE: ${entityContent.includes('<!DOCTYPE')}`);
      console.error(`  Has style-sheet: ${entityContent.includes('<style-sheet')}`);
    }

    // Check if entity file is a DSSSL stylesheet (has XML DOCTYPE wrapper)
    // If so, extract just the <style-specification-body> content
    if (entityContent.includes('<!DOCTYPE') && entityContent.includes('<style-sheet')) {
      try {
        // Parse DOCTYPE to get parameter entities
        const { entities: entityFileEntities, paramDefMap: entityFileParamMap } = parseEntities(entityContent, path.dirname(entityPath), searchPaths, catalog);

        if (process.env.DEBUG_TEMPLATE) {
          console.error(`DEBUG_TEMPLATE: Parsed ${entityFileEntities.length} entities from ${entityPath}`);
          for (const e of entityFileEntities) {
            console.error(`  - ${e.name}: ${e.value || e.systemId || e.publicId}`);
          }
        }

        // Merge parameter entity definitions from entity file
        for (const [name, value] of entityFileParamMap.entries()) {
          paramDefMap.set(name, value);
        }

        // Merge general entities from entity file into global entity list
        // Port from: SGML makes entities from included files available globally
        for (const subEntity of entityFileEntities) {
          // Check if already exists (first definition wins)
          const existing = entities.find(e => e.name === subEntity.name);
          if (!existing) {
            entities.push(subEntity);
            if (process.env.DEBUG_TEMPLATE) {
              console.error(`DEBUG_TEMPLATE: Added entity to global list: ${subEntity.name}`);
            }
          }

          // Resolve file-based entities and replace inline
          let subSystemId = subEntity.systemId;
          if (!subSystemId && subEntity.publicId) {
            subSystemId = catalog.resolve(subEntity.publicId) || undefined;
          }
          if (subSystemId) {
            const subEntityPath = resolveEntityPath(subSystemId, path.dirname(entityPath), searchPaths);
            const subEntityContent = fs.readFileSync(subEntityPath, 'utf-8');
            const subEntityRef = `&${subEntity.name};`;
            entityContent = entityContent.replace(subEntityRef, subEntityContent);
          }
        }

        // Process marked sections BEFORE extracting style-specification-body
        // This allows marked sections to be embedded in Scheme code
        entityContent = entityContent.replace(/<!\[%([\w-]+);?\[([\s\S]*?)\]\]>/g, (match, entityName, content) => {
          const entityValue = entityFileParamMap.get(entityName) || paramDefMap.get(entityName);
          const included = entityValue === 'INCLUDE';
          if (process.env.DEBUG_TEMPLATE) {
            console.error(`DEBUG_TEMPLATE: [Entity file - before extraction] Marked section <![%${entityName};[...]> - entity value: "${entityValue}", included: ${included}`);
          }
          return included ? content : '';
        });

        // Extract just the <style-specification-body> content
        const { content: extractedContent } = extractStyleSpecification(entityContent);
        entityContent = extractedContent;
      } catch (e) {
        // If extraction fails, use raw content
        if (process.env.DEBUG_TEMPLATE) {
          console.error(`DEBUG_TEMPLATE: Failed to extract style-specification from ${entityPath}: ${e}`);
        }
      }
    }

    // Strip CDATA sections if present (used in some templates for XML escaping)
    // Transform: <![CDATA[content]]> → content
    entityContent = entityContent.replace(/<!\[CDATA\[([\s\S]*?)\]\]>/g, '$1');

    // Port from: OpenJade processes SGML marked sections
    // Process marked sections based on parameter entity values
    // Pattern: <![%entity[ content ]]> or <![%entity;[ content ]]>
    // Note: Entity names can contain dashes (e.g., l10n-en), semicolon is optional
    entityContent = entityContent.replace(/<!\[%([\w-]+);?\[([\s\S]*?)\]\]>/g, (match, entityName, content) => {
      const entityValue = paramDefMap.get(entityName);
      const included = entityValue === 'INCLUDE';
      if (process.env.DEBUG_TEMPLATE) {
        console.error(`DEBUG_TEMPLATE: [Entity file] Marked section <![%${entityName};[...]> - entity value: "${entityValue}", included: ${included}`);
      }
      // Only include content if entity is explicitly set to "INCLUDE"
      // If not defined or set to "IGNORE", remove the entire marked section
      return included ? content : '';
    });

    const entityRef = `&${entity.name};`;
    entityReplacements.push({ ref: entityRef, content: entityContent, path: entityPath });
  }

  // Separate entity replacements into inline (text values) and file-based
  const inlineReplacements = entityReplacements.filter(r => !r.content.includes('\n'));
  const fileReplacements = entityReplacements.filter(r => r.content.includes('\n'));

  if (process.env.DEBUG_TEMPLATE) {
    console.error(`DEBUG_TEMPLATE: ${inlineReplacements.length} inline replacements, ${fileReplacements.length} file-based`);
    if (inlineReplacements.length > 0) {
      console.error(`DEBUG_TEMPLATE: First few inline replacements:`);
      for (let i = 0; i < Math.min(5, inlineReplacements.length); i++) {
        console.error(`  ${inlineReplacements[i].ref} -> "${inlineReplacements[i].content}"`);
      }
      // Check for TableofContents specifically
      const tocReplacement = inlineReplacements.find(r => r.ref === '&TableofContents;');
      if (tocReplacement) {
        console.error(`DEBUG_TEMPLATE: Found &TableofContents; -> "${tocReplacement.content}"`);
      } else {
        console.error(`DEBUG_TEMPLATE: &TableofContents; NOT found in inline replacements!`);
      }
    }
  }

  // NOTE: Inline replacements are applied to the combined code later (after prepending external specs)

  // Then process file-based entity references line by line to track line numbers
  const lines = schemeCode.split('\n');
  const resultLines: string[] = [];

  for (let i = 0; i < lines.length; i++) {
    let line = lines[i];
    let wasReplaced = false;

    // Check if this line contains file-based entity references
    for (const replacement of fileReplacements) {
      if (line.includes(replacement.ref)) {
        // Replace entity reference with content
        const entityLines = replacement.content.split('\n');

        // Add source map entry for the entity content
        if (entityLines.length > 0) {
          sourceMap.push({
            startLine: currentLine,
            endLine: currentLine + entityLines.length - 1,
            sourceFile: replacement.path,
            sourceLine: 0,
          });
        }

        // Add entity lines to result
        resultLines.push(...entityLines);
        currentLine += entityLines.length;
        wasReplaced = true;
        break; // Only one entity per line
      }
    }

    if (!wasReplaced) {
      // No entity reference, track as coming from template file
      sourceMap.push({
        startLine: currentLine,
        endLine: currentLine,
        sourceFile: templatePath,
        sourceLine: i + mainTemplateOffset - 1, // i is 0-based, convert 1-based mainTemplateOffset to 0-based
      });
      resultLines.push(line);
      currentLine++;
    }
  }

  // Combine external specs + main code
  let combinedCode = [...prependedLines, ...resultLines].join('\n');

  // Apply inline replacements to the ENTIRE combined code (including prepended external specs)
  // Port from: SGML expands all entity references before Scheme sees the code
  if (process.env.DEBUG_TEMPLATE) {
    console.error(`DEBUG_TEMPLATE: Applying ${inlineReplacements.length} inline replacements to combined code (${combinedCode.length} chars)`);
  }
  for (const replacement of inlineReplacements) {
    const regex = new RegExp(replacement.ref.replace(/[.*+?^${}()|[\]\\]/g, '\\$&'), 'g');
    const before = combinedCode;
    combinedCode = combinedCode.replace(regex, replacement.content);
    if (process.env.DEBUG_TEMPLATE && before !== combinedCode) {
      const count = (before.match(regex) || []).length;
      console.error(`DEBUG_TEMPLATE: Replaced ${count} occurrences of ${replacement.ref}`);
    }
  }

  return {
    schemeCode: combinedCode,
    entities,
    sourceMap,
  };
}

/**
 * Parse entity declarations from DOCTYPE and parameter entity files
 *
 * Port from: OpenJade handles multiple entity declaration patterns
 * Extracts:
 * - <!ENTITY name SYSTEM "file.scm">
 * - <!ENTITY name SYSTEM "file.scm" CDATA DSSSL>
 * - <!ENTITY name PUBLIC "pubid" "file.scm">
 * - <!ENTITY name PUBLIC "pubid" CDATA DSSSL>
 * Also processes parameter entity references to extract entities from those files
 */
function parseEntities(content: string, baseDir: string, searchPaths: string[], catalog: Catalog): { entities: Entity[]; paramDefMap: Map<string, string> } {
  const entities: Entity[] = [];

  // Match DOCTYPE declaration - case-insensitive, more flexible with whitespace
  const doctypeMatch = content.match(/<!DOCTYPE[\s\S]*?\[([\s\S]*?)\]/i);
  if (!doctypeMatch) {
    if (process.env.DEBUG_TEMPLATE) {
      console.error(`DEBUG_TEMPLATE: parseEntities - no DOCTYPE found`);
    }
    return { entities, paramDefMap: new Map() };
  }

  const doctype = doctypeMatch[1];

  if (process.env.DEBUG_TEMPLATE) {
    console.error(`DEBUG_TEMPLATE: parseEntities - found DOCTYPE with ${doctype.length} chars`);
  }

  // First, collect parameter entity references and load them
  // Pattern: <!ENTITY % name SYSTEM "file.ent"> OR <!ENTITY % name PUBLIC "pubid" "sysid">
  const paramEntityDefRegex = /<!ENTITY\s+%\s+([\w.-]+)\s+(?:SYSTEM\s+"([^"]+)"|PUBLIC\s+"[^"]+"\s+"([^"]+)")\s*>/gi;
  const paramEntities: Array<{ name: string; systemId: string }> = [];
  let match;

  while ((match = paramEntityDefRegex.exec(doctype)) !== null) {
    const name = match[1];
    const systemId = match[2] || match[3]; // SYSTEM or PUBLIC's system id
    paramEntities.push({
      name,
      systemId,
    });
    if (process.env.DEBUG_TEMPLATE) {
      console.error(`DEBUG_TEMPLATE: Found parameter entity def: %${name}; SYSTEM/PUBLIC "${systemId}"`);
    }
  }

  // Global map to track parameter entity values (INCLUDE/IGNORE)
  // This will be used to process marked sections in entity files
  const globalParamDefMap = new Map<string, string>();

  // Process parameter entity references (%name;) and parse entities from those files
  for (const paramEntity of paramEntities) {
    const refPattern = new RegExp(`%${paramEntity.name};`, 'g');
    if (refPattern.test(doctype)) {
      if (process.env.DEBUG_TEMPLATE) {
        console.error(`DEBUG_TEMPLATE: Found parameter entity reference %${paramEntity.name}; in DOCTYPE, loading ${paramEntity.systemId}`);
      }
      try {
        const paramPath = resolveEntityPath(paramEntity.systemId, baseDir, searchPaths);
        let paramContent = fs.readFileSync(paramPath, 'utf-8');

        if (process.env.DEBUG_TEMPLATE) {
          console.error(`DEBUG_TEMPLATE: Loaded ${paramPath} (${paramContent.length} bytes)`);
        }

        // Parse parameter entity definitions to track INCLUDE/IGNORE status
        // Pattern: <!ENTITY % name "value">
        const paramDefRegex = /<!ENTITY\s+%\s+([\w-]+)\s+"(INCLUDE|IGNORE)"\s*>/gi;
        let paramDefMatch;
        while ((paramDefMatch = paramDefRegex.exec(paramContent)) !== null) {
          globalParamDefMap.set(paramDefMatch[1], paramDefMatch[2]);
          if (process.env.DEBUG_TEMPLATE) {
            console.error(`DEBUG_TEMPLATE: Found parameter entity %${paramDefMatch[1]}; = "${paramDefMatch[2]}"`);
          }
        }

        if (process.env.DEBUG_TEMPLATE) {
          console.error(`DEBUG_TEMPLATE: Processing file ${paramPath}, found ${globalParamDefMap.size} parameter entities`);
        }

        // Process marked sections based on parameter entity values
        // Pattern: <![%entity[ content ]]> or <![%entity;[ content ]]>
        // Note: Entity names can contain dashes (e.g., l10n-en), semicolon is optional
        paramContent = paramContent.replace(/<!\[%([\w-]+);?\[([\s\S]*?)\]\]>/g, (match, entityName, content) => {
          const entityValue = globalParamDefMap.get(entityName);
          const included = entityValue === 'INCLUDE';
          if (process.env.DEBUG_TEMPLATE) {
            console.error(`DEBUG_TEMPLATE: Marked section <![%${entityName};[...]> - entity value: "${entityValue}", included: ${included}`);
          }
          // Only include content if entity is explicitly set to "INCLUDE"
          // If not defined or set to "IGNORE", remove the entire marked section
          return included ? content : '';
        });

        // Parse entity declarations from parameter entity file
        // Handle both file-based entities (SYSTEM/PUBLIC) and text replacement entities
        // Pattern 1: <!ENTITY name SYSTEM "file">
        // Pattern 2: <!ENTITY name PUBLIC "pubid" "sysid">
        // Pattern 3: <!ENTITY name "text value">
        const paramEntityRegex = /<!ENTITY\s+(?!%)([\w.-]+)\s+(?:SYSTEM\s+"([^"]+)"|PUBLIC\s+"([^"]+)"(?:\s+"([^"]+)")?|"([^"]*)")?\s*(?:CDATA\s+DSSSL)?\s*>/gi;
        let paramMatch;

        if (process.env.DEBUG_TEMPLATE) {
          console.error(`DEBUG_TEMPLATE: Parsing entities from ${paramPath} (${paramContent.length} bytes)`);
        }

        while ((paramMatch = paramEntityRegex.exec(paramContent)) !== null) {
          const name = paramMatch[1];
          const systemId = paramMatch[2] || paramMatch[4]; // SYSTEM id or PUBLIC's optional SYSTEM id
          const publicId = paramMatch[3]; // PUBLIC id
          const value = paramMatch[5]; // Direct text value

          // Check if entity already exists - if so, skip (first definition wins)
          // This handles marked sections: when INCLUDE, the entity inside the marked
          // section is parsed first, and the fallback definition should be ignored
          const existing = entities.find(e => e.name === name);
          if (existing) {
            if (process.env.DEBUG_TEMPLATE) {
              console.error(`DEBUG_TEMPLATE: Entity ${name} already defined, skipping duplicate`);
            }
            continue;
          }

          if (process.env.DEBUG_TEMPLATE) {
            console.error(`DEBUG_TEMPLATE: Entity parsed: ${name} -> ${systemId || publicId || value || '(no id/value)'}`);
          }

          entities.push({
            name,
            systemId,
            publicId,
            value,
          });
        }
      } catch (e) {
        // Parameter entity file not found - skip
      }
    }
  }

  // Parse regular entity declarations from DOCTYPE
  // Improved regex to capture both PUBLIC and SYSTEM identifiers
  const entityRegex = /<!ENTITY\s+(?!%)([\w.-]+)\s+(?:SYSTEM\s+"([^"]+)"|PUBLIC\s+"([^"]+)"(?:\s+"([^"]+)")?)?\s*(?:CDATA\s+DSSSL)?\s*>/gi;

  while ((match = entityRegex.exec(doctype)) !== null) {
    const name = match[1];
    const systemId = match[2] || match[4]; // SYSTEM id or PUBLIC's optional SYSTEM id
    const publicId = match[3]; // PUBLIC id

    entities.push({
      name,
      systemId,
      publicId,
    });
  }

  return { entities, paramDefMap: globalParamDefMap };
}

/**
 * Parse external specification declarations
 * Port from: DSSSL style-sheet - <external-specification id="..." document="...">
 */
function parseExternalSpecifications(content: string): Array<{ id: string; document: string }> {
  const specs: Array<{ id: string; document: string }> = [];

  // Match: <external-specification id="xxx" document="yyy">
  const specRegex = /<external-specification\s+id="([\w-]+)"\s+document="([^"]+)">/gi;
  let match;

  while ((match = specRegex.exec(content)) !== null) {
    specs.push({
      id: match[1],
      document: match[2],
    });
  }

  return specs;
}

/**
 * Parse the use attribute from style-specification
 * Port from: DSSSL style-sheet - <style-specification use="id1 id2 ...">
 */
function parseStyleSpecificationUse(content: string): string[] {
  // Match: <style-specification ... use="id1 id2 id3" ...>
  const match = content.match(/<style-specification[^>]*\s+use="([^"]+)"/i);

  if (!match) {
    return []; // No use attribute
  }

  // Split by whitespace to get individual spec IDs
  return match[1].trim().split(/\s+/);
}

/**
 * Extract content from <style-specification> tag
 * Case-insensitive to match OpenJade behavior
 * Also handles optional <style-specification-body> nested tag
 * Returns the extracted content and the line offset in the original file
 */
function extractStyleSpecification(content: string): { content: string; lineOffset: number } {
  // Match <style-specification>...</style-specification> (case-insensitive)
  const match = content.match(/<style-specification[^>]*>([\s\S]*?)<\/style-specification>/i);

  if (!match) {
    throw new Error('XML template must contain <style-specification> tag');
  }

  // Calculate line offset - how many lines before the match
  const beforeMatch = content.substring(0, match.index);
  const lineOffset = (beforeMatch.match(/\n/g) || []).length;

  let extracted = match[1];
  let additionalOffset = 0;

  // If there's a <style-specification-body> tag, extract its content
  const bodyMatch = extracted.match(/<style-specification-body[^>]*>([\s\S]*?)<\/style-specification-body>/i);
  if (bodyMatch) {
    // Add offset from style-specification to style-specification-body
    const beforeBody = extracted.substring(0, bodyMatch.index);
    additionalOffset = (beforeBody.match(/\n/g) || []).length;
    extracted = bodyMatch[1];
  }

  return {
    content: extracted,
    lineOffset: lineOffset + additionalOffset + 1, // +1 because we skip the opening tag line
  };
}

/**
 * Resolve entity system ID to file path
 *
 * Search order:
 * 1. Relative to base directory
 * 2. Search paths in order
 * 3. Absolute paths
 */
function resolveEntityPath(systemId: string, baseDir: string, searchPaths: string[]): string {
  // Try relative to base directory
  const relativePath = path.resolve(baseDir, systemId);
  if (fs.existsSync(relativePath)) {
    return relativePath;
  }

  // Try search paths
  for (const searchPath of searchPaths) {
    const searchedPath = path.resolve(searchPath, systemId);
    if (fs.existsSync(searchedPath)) {
      return searchedPath;
    }
  }

  // Try as absolute path
  if (path.isAbsolute(systemId) && fs.existsSync(systemId)) {
    return systemId;
  }

  throw new Error(`Cannot resolve entity: ${systemId}`);
}
