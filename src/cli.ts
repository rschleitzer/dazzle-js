#!/usr/bin/env node
// Copyright (c) 1996 James Clark
// See the file copying.txt for copying permission.

// dazzle - DSSSL processor CLI (copy of jade from @scaly/openjade)
// For testing and experimenting

import * as fs from 'fs';
import * as path from 'path';
import { createRequire } from 'module';
import {
  StringC,
  Char,
  CharsetInfo,
  UnivCharsetDesc,
  Vector,
  createExtendEntityManager,
  ExtendEntityManager,
  SOCatalogManager,
  String as StringOf
} from '@scaly/opensp';
import {
  DssslApp,
  FOTBuilder,
  FOTBuilderExtension,
  TransformFOTBuilder,
  FileOutputStream,
  SgmlFOTBuilder,
  makeRtfFOTBuilder,
  makeTeXFOTBuilder,
  makeMifFOTBuilder
} from '@scaly/openjade';

const require = createRequire(import.meta.url);

// Helper to create StringC from string
function makeStringC(s: string): StringC {
  const arr: Char[] = [];
  for (let i = 0; i < s.length; i++) {
    arr.push(s.charCodeAt(i));
  }
  return new StringOf<Char>(arr, arr.length);
}

// Helper to convert StringC to string
function stringCToString(sc: StringC): string {
  if (!sc.ptr_ || sc.length_ === 0) return '';
  let result = '';
  for (let i = 0; i < sc.length_; i++) {
    result += String.fromCharCode(sc.ptr_[i]);
  }
  return result;
}

// Output types - matches upstream jade.cxx
enum OutputType {
  fotType = 0,
  rtfType = 1,
  texType = 2,
  sgmlType = 3,
  xmlType = 4,
  mifType = 5
}

const outputTypeNames = ['fot', 'rtf', 'tex', 'sgml', 'xml', 'mif'];

// Units per inch (from upstream: 72000)
const UNITS_PER_INCH = 72000;

// JadeApp - the jade application
// Ported from upstream openjade/jade/jade.cxx
class JadeApp extends DssslApp {
  private outputType_: OutputType = OutputType.fotType;
  private outputFilename_: string = '';
  private outputOptions_: StringC[] = [];

  constructor() {
    super(UNITS_PER_INCH);
  }

  // Process command-line option
  override processOption(opt: string, arg: string | null): void {
    switch (opt) {
      case 't':
        if (arg) {
          // Parse output type, possibly with sub-options like "xml-raw"
          const dashIdx = arg.indexOf('-');
          const typeName = dashIdx >= 0 ? arg.substring(0, dashIdx) : arg;

          const typeIdx = outputTypeNames.indexOf(typeName);
          if (typeIdx < 0) {
            console.error(`Unknown output type: ${typeName}`);
          } else {
            this.outputType_ = typeIdx as OutputType;
          }

          // Parse sub-options after '-'
          if (dashIdx >= 0) {
            const subOpts = arg.substring(dashIdx + 1).split('-');
            for (const subOpt of subOpts) {
              if (subOpt) {
                this.outputOptions_.push(makeStringC(subOpt));
              }
            }
          }
        }
        break;
      case 'o':
        if (arg) {
          if (arg.length === 0) {
            console.error('Empty output filename');
          } else {
            this.outputFilename_ = arg;
          }
        }
        break;
      default:
        super.processOption(opt, arg);
        break;
    }
  }

  // Create the FOTBuilder based on output type
  makeFOTBuilder(exts: { value: FOTBuilderExtension[] | null }): FOTBuilder | null {
    // Determine output filename if not specified
    if (this.outputFilename_.length === 0) {
      const baseName = stringCToString(this.defaultOutputBasename_);
      if (baseName.length > 0) {
        this.outputFilename_ = baseName;
      } else {
        this.outputFilename_ = 'jade-out';
      }
      this.outputFilename_ += '.' + outputTypeNames[this.outputType_];
    }

    switch (this.outputType_) {
      case OutputType.sgmlType:
      case OutputType.xmlType: {
        // TransformFOTBuilder outputs SGML/XML
        const isXml = this.outputType_ === OutputType.xmlType;
        return makeTransformFOTBuilder(
          this,
          this.outputFilename_,
          isXml,
          this.outputOptions_,
          exts
        );
      }

      case OutputType.fotType: {
        // FOT output - use SgmlFOTBuilder to output flow object tree as SGML
        const fotOutputStream = new FileOutputStream(this.outputFilename_);
        const fotBuilder = new SgmlFOTBuilder(
          (s: string) => fotOutputStream.write(s),
          () => fotOutputStream.flush()
        );
        return fotBuilder;
      }

      case OutputType.rtfType: {
        // RTF output - upstream uses twips (1/20 point = 1/1440 inch)
        this.unitsPerInch_ = 20 * 72;  // 1440 twips per inch
        const rtfOutputStream = new FileOutputStream(this.outputFilename_);
        const rtfBuilder = makeRtfFOTBuilder(
          (s: string) => rtfOutputStream.write(s),
          () => rtfOutputStream.flush(),
          [],
          exts
        );
        return rtfBuilder;
      }

      case OutputType.texType: {
        // TeX output
        const texOutputStream = new FileOutputStream(this.outputFilename_);
        const texBuilder = makeTeXFOTBuilder(
          (s: string) => texOutputStream.write(s),
          () => texOutputStream.flush(),
          [],
          exts
        );
        return texBuilder;
      }

      case OutputType.mifType: {
        // MIF output
        const mifOutputStream = new FileOutputStream(this.outputFilename_);
        const mifBuilder = makeMifFOTBuilder(
          (s: string) => mifOutputStream.write(s),
          () => mifOutputStream.flush(),
          [],
          exts
        );
        return mifBuilder;
      }

      default:
        console.error('Unknown output type');
        return null;
    }
  }
}

// Create TransformFOTBuilder for SGML/XML output
function makeTransformFOTBuilder(
  _mgr: DssslApp,
  outputFilename: string,
  isXml: boolean,
  options: StringC[],
  _exts: { value: FOTBuilderExtension[] | null }
): FOTBuilder | null {
  // Create file output stream
  const outputStream = new FileOutputStream(outputFilename);

  // Convert options to string array
  const optionStrs: string[] = [];
  for (const opt of options) {
    optionStrs.push(stringCToString(opt));
  }

  return new TransformFOTBuilder(outputStream, isXml, optionStrs);
}

// Parse command line arguments
function parseArgs(args: string[]): {
  files: string[];
  dssslSpec: string | null;
  outputType: string | null;
  outputFile: string | null;
  catalogs: string[];
  defineVars: string[];
  debugMode: boolean;
  dsssl2: boolean;
  strictMode: boolean;
} {
  const result = {
    files: [] as string[],
    dssslSpec: null as string | null,
    outputType: null as string | null,
    outputFile: null as string | null,
    catalogs: [] as string[],
    defineVars: [] as string[],
    debugMode: false,
    dsssl2: false,
    strictMode: false
  };

  for (let i = 0; i < args.length; i++) {
    const arg = args[i];

    if (arg.startsWith('-')) {
      const opt = arg.substring(1);

      switch (opt) {
        case 'd':
          if (i + 1 < args.length) {
            result.dssslSpec = args[++i];
          }
          break;
        case 't':
          if (i + 1 < args.length) {
            result.outputType = args[++i];
          }
          break;
        case 'o':
          if (i + 1 < args.length) {
            result.outputFile = args[++i];
          }
          break;
        case 'c':
        case 'm':
          if (i + 1 < args.length) {
            result.catalogs.push(args[++i]);
          }
          break;
        case 'V':
          if (i + 1 < args.length) {
            result.defineVars.push(args[++i]);
          }
          break;
        case 'G':
          result.debugMode = true;
          break;
        case '2':
          result.dsssl2 = true;
          break;
        case 's':
          result.strictMode = true;
          break;
        case 'v':
        case '-version':
          console.log('dazzle 0.1.0 (based on @scaly/openjade)');
          process.exit(0);
          break;
        case 'h':
        case '-help':
          printUsage();
          process.exit(0);
          break;
        default:
          console.error(`Unknown option: ${arg}`);
          break;
      }
    } else {
      result.files.push(arg);
    }
  }

  return result;
}

function printUsage(): void {
  console.log(`Usage: dazzle [OPTIONS] FILE...

DSSSL processor - transforms SGML/XML documents using DSSSL stylesheets.

Options:
  -d SPEC         DSSSL specification file
  -t TYPE         Output type: fot, rtf, tex, sgml, xml (default: fot)
                  Can include options: -t xml-raw
  -o FILE         Output filename
  -c FILE         Use catalog file
  -V VAR[=VAL]    Define variable
  -G              Debug mode
  -2              Enable DSSSL2 extensions
  -s              Strict mode
  -v, --version   Show version
  -h, --help      Show this help

Examples:
  dazzle -t xml -d style.dsl doc.sgml
  dazzle -t sgml -o output.sgml -d style.dsl doc.sgml
`);
}

// Main entry point
function main(): number {
  const args = process.argv.slice(2);
  const parsed = parseArgs(args);

  if (parsed.files.length === 0) {
    console.error('No input files specified');
    printUsage();
    return 1;
  }

  // Create entity manager with Unicode charset
  const range = { descMin: 0, count: 0x110000, univMin: 0 };
  const desc = new UnivCharsetDesc([range], 1);
  const charset = new CharsetInfo(desc);

  // File reader function
  const fileReader = (filePath: string): Uint8Array | null => {
    try {
      const data = fs.readFileSync(filePath);
      // Skip UTF-8 BOM if present (EF BB BF)
      if (data.length >= 3 && data[0] === 0xEF && data[1] === 0xBB && data[2] === 0xBF) {
        return data.subarray(3);
      }
      return data;
    } catch {
      return null;
    }
  };

  const entityManager = createExtendEntityManager(charset, fileReader);

  // Set up catalog manager - port of EntityApp::entityManager() from EntityApp.cxx
  // The upstream uses SGML_CATALOG_FILES_DEFAULT which is compiled in at build time.
  // MacPorts sets it to /opt/local/share/sgml/catalog which chains to openjade's catalog.
  // For our port, we add the bundled DSSSL catalog as a fallback after user catalogs.
  const catalogSysids = new Vector<StringC>();

  // Add user-specified catalogs (these are explicitly requested by -c option)
  for (const cat of parsed.catalogs) {
    catalogSysids.push_back(makeStringC(cat));
  }

  // Add bundled DSSSL catalog as fallback (like SGML_CATALOG_FILES_DEFAULT in upstream)
  // This provides the DSSSL DTD public ID mappings needed for parsing stylesheets
  // Look for the catalog in @scaly/openjade package
  const openjadePath = path.dirname(require.resolve('@scaly/openjade/package.json'));
  const builtinCatalog = path.join(openjadePath, 'dsssl', 'catalog');
  if (fs.existsSync(builtinCatalog)) {
    catalogSysids.push_back(makeStringC(builtinCatalog));
  }

  // Always create catalog manager with useDocCatalog=true (like upstream)
  // This ensures each file's directory is searched for a catalog
  const catalogManager = SOCatalogManager.make(
    catalogSysids,
    parsed.catalogs.length,  // Number of user catalogs that must exist
    charset,
    charset,
    true  // useDocCatalog - look for catalog in each file's directory
  );
  (entityManager as ExtendEntityManager).setCatalogManager(catalogManager);

  // Create JadeApp
  const app = new JadeApp();
  app.setEntityManager(entityManager);

  // Process options
  if (parsed.dssslSpec) {
    app.processOption('d', parsed.dssslSpec);
  }
  if (parsed.outputType) {
    app.processOption('t', parsed.outputType);
  }
  if (parsed.outputFile) {
    app.processOption('o', parsed.outputFile);
  }
  for (const v of parsed.defineVars) {
    app.processOption('V', v);
  }
  if (parsed.debugMode) {
    app.processOption('G', null);
  }
  if (parsed.dsssl2) {
    app.processOption('2', null);
  }
  if (parsed.strictMode) {
    app.processOption('s', null);
  }

  // Process each input file
  let exitCode = 0;
  for (const file of parsed.files) {
    const sysid = makeStringC(file);
    const ret = app.processSysid(sysid);
    if (ret !== 0) {
      exitCode = ret;
      continue;
    }

    // Process the grove with DSSSL
    app.processGrove();
  }

  return exitCode;
}

// Run
const exitCode = main();
process.exit(exitCode);
