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
  const entities = parseEntities(content, baseDir, searchPaths, catalog);

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
        systemId = catalog.resolve(entity.publicId) || undefined;
      }

      if (!systemId) {
        // Entity has neither SYSTEM path nor resolvable PUBLIC identifier
        console.warn(`Warning: External specification "${spec.id}" references entity "${spec.document}" that cannot be resolved - skipping`);
        continue;
      }

      const specPath = resolveEntityPath(systemId, baseDir, searchPaths);
      let specContent = fs.readFileSync(specPath, 'utf-8');
      let lineOffset = 0;

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

    // Add source map entry for this external spec
    sourceMap.push({
      startLine: currentLine,
      endLine: currentLine + lines.length - 1,
      sourceFile: external.path,
      sourceLine: external.lineOffset - 1, // Convert 1-based lineOffset to 0-based sourceLine
    });

    currentLine += lines.length;
  }

  // Track all entity references and their positions
  const entityReplacements: Array<{ ref: string; content: string; path: string }> = [];

  for (const entity of entities) {
    // Resolve entity path - try SYSTEM first, then PUBLIC via catalog
    let systemId = entity.systemId;
    if (!systemId && entity.publicId) {
      // Try resolving PUBLIC identifier via catalog
      systemId = catalog.resolve(entity.publicId) || undefined;
    }

    if (!systemId) {
      // Skip entities that cannot be resolved
      continue;
    }

    const entityPath = resolveEntityPath(systemId, baseDir, searchPaths);
    let entityContent = fs.readFileSync(entityPath, 'utf-8');

    // Strip CDATA sections if present (used in some templates for XML escaping)
    // Transform: <![CDATA[content]]> → content
    entityContent = entityContent.replace(/<!\[CDATA\[([\s\S]*?)\]\]>/g, '$1');

    // Port from: OpenJade processes SGML marked sections
    // Strip marked sections for INCLUDE (keep content): <![%entity;[ content ]]> → content
    // Pattern matches: <![%name;[ ... ]]> or <![%name[ ... ]]>
    entityContent = entityContent.replace(/<!\[%[\w-]+;?\[\s*([\s\S]*?)\s*\]\]>/g, '$1');

    const entityRef = `&${entity.name};`;
    entityReplacements.push({ ref: entityRef, content: entityContent, path: entityPath });
  }

  // Replace entity references and build source map
  // Process line by line to track line numbers
  const lines = schemeCode.split('\n');
  const resultLines: string[] = [];

  for (let i = 0; i < lines.length; i++) {
    let line = lines[i];
    let wasReplaced = false;

    // Check if this line contains entity references
    for (const replacement of entityReplacements) {
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
  const allLines = [...prependedLines, ...resultLines];

  return {
    schemeCode: allLines.join('\n'),
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
function parseEntities(content: string, baseDir: string, searchPaths: string[], catalog: Catalog): Entity[] {
  const entities: Entity[] = [];

  // Match DOCTYPE declaration - case-insensitive, more flexible with whitespace
  const doctypeMatch = content.match(/<!DOCTYPE[\s\S]*?\[([\s\S]*?)\]/i);
  if (!doctypeMatch) {
    return entities;
  }

  const doctype = doctypeMatch[1];

  // First, collect parameter entity references and load them
  // Pattern: <!ENTITY % name SYSTEM "file.ent"> followed by %name;
  const paramEntityDefRegex = /<!ENTITY\s+%\s+([\w.-]+)\s+SYSTEM\s+"([^"]+)"\s*>/gi;
  const paramEntities: Array<{ name: string; systemId: string }> = [];
  let match;

  while ((match = paramEntityDefRegex.exec(doctype)) !== null) {
    paramEntities.push({
      name: match[1],
      systemId: match[2],
    });
  }

  // Process parameter entity references (%name;) and parse entities from those files
  for (const paramEntity of paramEntities) {
    const refPattern = new RegExp(`%${paramEntity.name};`, 'g');
    if (refPattern.test(doctype)) {
      try {
        const paramPath = resolveEntityPath(paramEntity.systemId, baseDir, searchPaths);
        let paramContent = fs.readFileSync(paramPath, 'utf-8');

        // Strip marked sections to get entity declarations
        paramContent = paramContent.replace(/<!\[%[\w-]+;?\[\s*([\s\S]*?)\s*\]\]>/g, '$1');

        // Parse entity declarations from parameter entity file
        // Improved regex to capture both PUBLIC and SYSTEM identifiers
        const paramEntityRegex = /<!ENTITY\s+(?!%)([\w.-]+)\s+(?:SYSTEM\s+"([^"]+)"|PUBLIC\s+"([^"]+)"(?:\s+"([^"]+)")?)?\s*(?:CDATA\s+DSSSL)?\s*>/gi;
        let paramMatch;

        while ((paramMatch = paramEntityRegex.exec(paramContent)) !== null) {
          const name = paramMatch[1];
          const systemId = paramMatch[2] || paramMatch[4]; // SYSTEM id or PUBLIC's optional SYSTEM id
          const publicId = paramMatch[3]; // PUBLIC id

          entities.push({
            name,
            systemId,
            publicId,
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

  return entities;
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
