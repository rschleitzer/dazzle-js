/**
 * SGML/XML Catalog Support
 * Port from: OpenJade sgmlnorm/catalog.cxx
 *
 * Implements basic OASIS catalog format for resolving PUBLIC identifiers.
 * Format specification: OASIS TR 9401:1997
 *
 * Supported entries:
 * - PUBLIC "pubid" "systemid"
 * - CATALOG "catalogfile"
 * - OVERRIDE YES|NO
 * - -- comments
 */

import * as fs from 'fs';
import * as path from 'path';

/**
 * Catalog entry types
 */
interface PublicEntry {
  type: 'PUBLIC';
  publicId: string;
  systemId: string;
}

interface CatalogEntry {
  type: 'CATALOG';
  catalogPath: string;
}

type CatalogEntryType = PublicEntry | CatalogEntry;

/**
 * Catalog class - Maps PUBLIC identifiers to SYSTEM identifiers
 * Port from: OpenJade Catalog class
 */
export class Catalog {
  private entries: Map<string, string> = new Map();
  private override: boolean = true; // Default to YES
  private loaded: Set<string> = new Set(); // Track loaded catalogs to avoid cycles

  /**
   * Load a catalog file
   * Port from: OpenJade Catalog::parseCatalog()
   *
   * @param catalogPath Path to catalog file
   * @param baseDir Base directory for resolving relative paths
   */
  load(catalogPath: string, baseDir: string): void {
    // Resolve catalog path
    const resolvedPath = path.isAbsolute(catalogPath)
      ? catalogPath
      : path.resolve(baseDir, catalogPath);

    // Avoid loading the same catalog twice (prevent cycles)
    if (this.loaded.has(resolvedPath)) {
      return;
    }

    if (!fs.existsSync(resolvedPath)) {
      // Catalog file not found - not an error, just skip
      return;
    }

    this.loaded.add(resolvedPath);

    const content = fs.readFileSync(resolvedPath, 'utf-8');
    const catalogDir = path.dirname(resolvedPath);

    // Parse catalog line by line
    const lines = content.split('\n');
    for (let i = 0; i < lines.length; i++) {
      let line = lines[i].trim();

      // Skip empty lines
      if (!line) {
        continue;
      }

      // Skip comments (-- to end of line)
      if (line.startsWith('--')) {
        continue;
      }

      // Remove inline comments
      const commentIndex = line.indexOf('--');
      if (commentIndex !== -1) {
        line = line.substring(0, commentIndex).trim();
      }

      // Parse entry
      if (line.startsWith('OVERRIDE')) {
        // OVERRIDE YES|NO
        const match = line.match(/^OVERRIDE\s+(YES|NO)/i);
        if (match) {
          this.override = match[1].toUpperCase() === 'YES';
        }
      } else if (line.startsWith('PUBLIC')) {
        // PUBLIC "pubid" "systemid"
        // May span multiple lines
        let fullLine = line;
        while (!this.hasCompletePublicEntry(fullLine) && i + 1 < lines.length) {
          i++;
          fullLine += ' ' + lines[i].trim();
        }

        const entry = this.parsePublicEntry(fullLine);
        if (entry) {
          // Resolve system ID relative to catalog directory
          const systemId = path.isAbsolute(entry.systemId)
            ? entry.systemId
            : path.resolve(catalogDir, entry.systemId);

          // Add to entries if not already present (or if override is YES)
          if (this.override || !this.entries.has(entry.publicId)) {
            this.entries.set(entry.publicId, systemId);
          }
        }
      } else if (line.startsWith('CATALOG')) {
        // CATALOG "catalogfile"
        const match = line.match(/^CATALOG\s+"([^"]+)"/);
        if (match) {
          // Recursively load referenced catalog
          this.load(match[1], catalogDir);
        } else {
          // Try without quotes
          const match2 = line.match(/^CATALOG\s+(.+)$/);
          if (match2) {
            this.load(match2[1].trim(), catalogDir);
          }
        }
      }
    }
  }

  /**
   * Check if line has complete PUBLIC entry
   */
  private hasCompletePublicEntry(line: string): boolean {
    // Try to match the full pattern
    return /^PUBLIC\s+"[^"]+"\s+(?:"[^"]+"|[^\s]+)/.test(line);
  }

  /**
   * Parse PUBLIC entry
   * Port from: OpenJade catalog parsing
   */
  private parsePublicEntry(line: string): PublicEntry | null {
    // Match: PUBLIC "pubid" systemid
    // System ID may or may not be quoted
    const match = line.match(/^PUBLIC\s+"([^"]+)"\s+(?:"([^"]+)"|(\S+))/);
    if (!match) {
      return null;
    }

    return {
      type: 'PUBLIC',
      publicId: match[1],
      systemId: match[2] || match[3], // Quoted or unquoted
    };
  }

  /**
   * Resolve a PUBLIC identifier to a SYSTEM identifier
   * Port from: OpenJade Catalog::resolve()
   *
   * @param publicId The PUBLIC identifier to resolve
   * @returns The SYSTEM identifier (file path) or null if not found
   */
  resolve(publicId: string): string | null {
    return this.entries.get(publicId) || null;
  }

  /**
   * Load catalogs from search paths
   * Port from: OpenJade command-line catalog handling
   *
   * @param searchPaths Directories to search for 'catalog' files
   * @returns Loaded catalog
   */
  static loadFromSearchPaths(searchPaths: string[]): Catalog {
    const catalog = new Catalog();

    for (const searchPath of searchPaths) {
      const catalogPath = path.join(searchPath, 'catalog');
      if (fs.existsSync(catalogPath)) {
        catalog.load(catalogPath, searchPath);
      }
    }

    return catalog;
  }
}
