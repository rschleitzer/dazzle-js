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

/**
 * Entity declaration from DOCTYPE
 */
interface Entity {
  name: string;
  systemId: string;
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
 * @param searchPaths Additional search paths for entity resolution
 * @returns Scheme code ready for parsing
 */
export function loadTemplate(templatePath: string, searchPaths: string[] = []): TemplateResult {
  const content = fs.readFileSync(templatePath, 'utf-8');
  const templateDir = path.dirname(templatePath);

  // Detect format
  if (content.trim().startsWith('<?xml') || content.trim().startsWith('<')) {
    // XML format - parse and extract
    return loadXmlTemplate(content, templatePath, templateDir, searchPaths);
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
function loadXmlTemplate(content: string, templatePath: string, baseDir: string, searchPaths: string[]): TemplateResult {
  // Parse DOCTYPE for entity declarations
  const entities = parseEntities(content);

  // Resolve entity references in content
  let schemeCode = extractStyleSpecification(content);

  // Build source map as we replace entity references
  const sourceMap: SourceMapEntry[] = [];
  let currentLine = 1;

  // Track all entity references and their positions
  const entityReplacements: Array<{ ref: string; content: string; path: string }> = [];

  for (const entity of entities) {
    const entityPath = resolveEntityPath(entity.systemId, baseDir, searchPaths);
    let entityContent = fs.readFileSync(entityPath, 'utf-8');

    // Strip CDATA sections if present (used in some templates for XML escaping)
    // Transform: <![CDATA[content]]> → content
    entityContent = entityContent.replace(/<!\[CDATA\[([\s\S]*?)\]\]>/g, '$1');

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
        sourceLine: i,
      });
      resultLines.push(line);
      currentLine++;
    }
  }

  return {
    schemeCode: resultLines.join('\n'),
    entities,
    sourceMap,
  };
}

/**
 * Parse entity declarations from DOCTYPE
 *
 * Extracts: <!ENTITY name SYSTEM "file.scm">
 */
function parseEntities(content: string): Entity[] {
  const entities: Entity[] = [];

  // Match DOCTYPE declaration - case-insensitive, more flexible with whitespace
  const doctypeMatch = content.match(/<!DOCTYPE[\s\S]*?\[([\s\S]*?)\]/i);
  if (!doctypeMatch) {
    return entities;
  }

  const doctype = doctypeMatch[1];

  // Match entity declarations: <!ENTITY name SYSTEM "file"> (case-insensitive, optional whitespace)
  const entityRegex = /<!ENTITY\s+(\w+)\s+SYSTEM\s+"([^"]+)"\s*>/gi;
  let match;

  while ((match = entityRegex.exec(doctype)) !== null) {
    entities.push({
      name: match[1],
      systemId: match[2],
    });
  }

  return entities;
}

/**
 * Extract content from <style-specification> tag
 * Case-insensitive to match OpenJade behavior
 * Also handles optional <style-specification-body> nested tag
 */
function extractStyleSpecification(content: string): string {
  // Match <style-specification>...</style-specification> (case-insensitive)
  const match = content.match(/<style-specification[^>]*>([\s\S]*?)<\/style-specification>/i);

  if (!match) {
    throw new Error('XML template must contain <style-specification> tag');
  }

  let extracted = match[1];

  // If there's a <style-specification-body> tag, extract its content
  const bodyMatch = extracted.match(/<style-specification-body[^>]*>([\s\S]*?)<\/style-specification-body>/i);
  if (bodyMatch) {
    extracted = bodyMatch[1];
  }

  return extracted;
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
