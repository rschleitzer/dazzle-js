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
 * Template loader result
 */
export interface TemplateResult {
  schemeCode: string;
  entities: Entity[];
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
    return loadXmlTemplate(content, templateDir, searchPaths);
  } else {
    // Plain Scheme format
    return {
      schemeCode: content,
      entities: [],
    };
  }
}

/**
 * Load XML-wrapped template
 */
function loadXmlTemplate(content: string, baseDir: string, searchPaths: string[]): TemplateResult {
  // Parse DOCTYPE for entity declarations
  const entities = parseEntities(content);

  // Resolve entity references in content
  let schemeCode = extractStyleSpecification(content);

  // Replace entity references with file contents
  for (const entity of entities) {
    const entityPath = resolveEntityPath(entity.systemId, baseDir, searchPaths);
    let entityContent = fs.readFileSync(entityPath, 'utf-8');

    // Strip CDATA sections if present (used in some templates for XML escaping)
    // Transform: <![CDATA[content]]> → content
    entityContent = entityContent.replace(/<!\[CDATA\[([\s\S]*?)\]\]>/g, '$1');

    // Replace &entityName; with content
    const entityRef = `&${entity.name};`;
    schemeCode = schemeCode.replace(new RegExp(entityRef, 'g'), entityContent);
  }

  return {
    schemeCode,
    entities,
  };
}

/**
 * Parse entity declarations from DOCTYPE
 *
 * Extracts: <!ENTITY name SYSTEM "file.scm">
 */
function parseEntities(content: string): Entity[] {
  const entities: Entity[] = [];

  // Match DOCTYPE declaration - more flexible with whitespace
  const doctypeMatch = content.match(/<!DOCTYPE[\s\S]*?\[([\s\S]*?)\]/);
  if (!doctypeMatch) {
    return entities;
  }

  const doctype = doctypeMatch[1];

  // Match entity declarations: <!ENTITY name SYSTEM "file"> (with optional whitespace)
  const entityRegex = /<!ENTITY\s+(\w+)\s+SYSTEM\s+"([^"]+)"\s*>/g;
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
 */
function extractStyleSpecification(content: string): string {
  // Match <style-specification>...</style-specification> (case-insensitive)
  const match = content.match(/<style-specification[^>]*>([\s\S]*?)<\/style-specification>/i);

  if (!match) {
    throw new Error('XML template must contain <style-specification> tag');
  }

  return match[1];
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
