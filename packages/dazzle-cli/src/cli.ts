#!/usr/bin/env node
/**
 * Dazzle CLI - OpenJade-compatible command line interface
 *
 * Port from: OpenJade jade/jade.cxx
 */

import { Command } from 'commander';
import * as fs from 'fs';
import * as path from 'path';
import { parseXmlGrove, parse, Compiler, GlobalEnvironment, Environment, VM, type ELObj, ProcessContext, loadTemplate } from 'dazzle-core';
import { createTransformBackend } from 'dazzle-backend-sgml';

/**
 * Format a Scheme value for display
 */
function formatValue(value: ELObj): string {
  // String
  const str = value.asString();
  if (str) return str.value;

  // Number
  const num = value.asNumber();
  if (num) return num.value.toString();

  // Boolean
  const bool = value.asBoolean();
  if (bool !== null) return bool.value ? '#t' : '#f';

  // Nil (empty list)
  if (value.asNil()) return '()';

  // List
  const pair = value.asPair();
  if (pair) {
    const elements: string[] = [];
    let current = value;

    while (current.asPair()) {
      const p = current.asPair()!;
      elements.push(formatValue(p.car));
      current = p.cdr;
    }

    // Check for improper list
    if (!current.asNil()) {
      return `(${elements.join(' ')} . ${formatValue(current)})`;
    }

    return `(${elements.join(' ')})`;
  }

  // Symbol
  const sym = value.asSymbol();
  if (sym) return sym.name;

  // Sosofo
  const sosofo = value.asSosofo();
  if (sosofo) return `<sosofo:${sosofo.type}>`;

  // Other
  return '<value>';
}

const program = new Command();

program
  .name('dazzle')
  .description('Template-driven code generation powered by Scheme and XML (DSSSL)')
  .version('0.1.0')
  .option('-d, --template <file>', 'Template file (required)')
  .option('-t, --backend <type>', 'Backend type (sgml)', 'sgml')
  .option('-V, --variable <key=value...>', 'Define variable', [])
  .option('-D, --search-path <dir...>', 'Add search path', [])
  .option('-o, --output <dir>', 'Output directory', '.')
  .argument('[input]', 'Input XML file')
  .action(async (inputFile: string | undefined, options: any) => {
    try {
      // Validate required options
      if (!options.template) {
        console.error('Error: Template file (-d) is required');
        process.exit(1);
      }

      if (!inputFile) {
        console.error('Error: Input XML file is required');
        process.exit(1);
      }

      // Check files exist
      if (!fs.existsSync(options.template)) {
        console.error(`Error: Template file not found: ${options.template}`);
        process.exit(1);
      }

      if (!fs.existsSync(inputFile)) {
        console.error(`Error: Input file not found: ${inputFile}`);
        process.exit(1);
      }

      // Load template (handles both plain Scheme and XML wrapper)
      const templatePath = path.resolve(options.template);
      const searchPaths = (options.searchPath || []).map((p: string) => path.resolve(p));
      const templateResult = loadTemplate(templatePath, searchPaths);
      const templateContent = templateResult.schemeCode;

      // Read input XML
      const inputPath = path.resolve(inputFile);
      const inputContent = fs.readFileSync(inputPath, 'utf-8');

      // Parse input XML to grove with DTD loading enabled
      // Port from: OpenJade uses OpenSP which always loads DTDs and applies defaults
      const grove = parseXmlGrove(inputContent, {
        baseUrl: inputPath,
        dtdload: true,   // Load external DTD
        dtdattr: true,   // Apply DTD default attributes
      });

      // Create backend
      const outputDir = path.resolve(options.output);
      const backend = createTransformBackend(outputDir);

      // Parse template into S-expressions
      const exprs = parse(templateContent);

      console.log(`Loaded entities: ${templateResult.entities.map(e => e.name).join(', ') || 'none'}`);

      if (process.env.DEBUG_TEMPLATE) {
        console.log('Template content:\n', templateContent);
      }

      // Create global environment and compiler
      const globals = new GlobalEnvironment();
      const compiler = new Compiler(globals);
      const env = new Environment();

      // Create VM and set up DSSSL context
      const vm = new VM();
      vm.globals = globals;
      vm.grove = grove;
      vm.currentNode = grove.root();

      // Create ProcessContext for sosofo execution
      const processContext = new ProcessContext(backend, vm);

      console.log(`Template: ${templatePath}`);
      console.log(`Input: ${inputPath}`);
      console.log(`Backend: ${options.backend}`);
      console.log(`Output dir: ${outputDir}`);
      console.log(`Grove root: ${grove.root().gi()}`);
      console.log(`Parsed ${exprs.length} expression(s) from template\n`);

      // Execute each expression (defines rules, functions, etc.)
      for (let i = 0; i < exprs.length; i++) {
        const expr = exprs[i];
        console.log(`Executing expression ${i + 1}...`);

        // Compile expression to bytecode
        const bytecode = compiler.compile(expr, env, 0, null);

        // Execute bytecode
        const result = vm.eval(bytecode);

        console.log(`Result: ${formatValue(result)}`);
      }

      // After loading all definitions, automatically invoke processing
      // Port from: OpenJade jade.cxx - automatically starts root processing
      console.log('\nStarting document processing...');

      const processRootFunc = globals.lookup('process-root');
      if (processRootFunc) {
        const func = processRootFunc.asFunction();
        if (func && func.isPrimitive()) {
          const sosofo = func.callPrimitive([], vm);

          // Process the resulting sosofo through the backend
          const sosofoParsed = sosofo.asSosofo();
          if (sosofoParsed) {
            processContext.process(sosofoParsed);
          }
        }
      }

      // Finish backend processing
      backend.end();

    } catch (error) {
      if (error instanceof Error) {
        console.error('Error:', error.message);
        if (process.env.DEBUG) {
          console.error(error.stack);
        }
      }
      process.exit(1);
    }
  });

program.parse();
