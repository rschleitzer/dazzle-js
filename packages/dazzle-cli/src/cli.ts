#!/usr/bin/env node
/**
 * Dazzle CLI - OpenJade-compatible command line interface
 *
 * Port from: OpenJade jade/jade.cxx
 */

import { Command } from 'commander';
import * as fs from 'fs';
import * as path from 'path';
import { parseXmlGrove, parse, Compiler, GlobalEnvironment, Environment, VM, ProcessContext, loadTemplate, type FotBuilder } from 'dazzle-core';
import { createTransformBackend } from 'dazzle-backend-sgml';
import { createFotBackend } from 'dazzle-backend-fot';

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

      if (!inputFile || inputFile.trim() === '') {
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
      const sourceMap = templateResult.sourceMap;

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

      // Create backend based on type
      const outputDir = path.resolve(options.output);
      let backend: FotBuilder;
      if (options.backend === 'fot') {
        backend = createFotBackend();
      } else if (options.backend === 'sgml') {
        backend = createTransformBackend(outputDir);
      } else {
        console.error(`Error: Unknown backend type: ${options.backend}`);
        process.exit(1);
      }

      // Parse template into S-expressions with source file tracking and source map
      const exprs = parse(templateContent, templatePath, sourceMap);

      if (process.env.DEBUG_TEMPLATE) {
        console.log('Template content:\n', templateContent);
      }

      // Build line number map for each expression
      // Simple heuristic: count newlines before each top-level '('
      const exprLineNumbers: number[] = [];
      let line = 1;
      let inExpr = 0;
      for (let i = 0; i < templateContent.length; i++) {
        const ch = templateContent[i];
        if (ch === '\n') {
          line++;
        } else if (ch === '(') {
          if (inExpr === 0) {
            // Starting a new top-level expression
            exprLineNumbers.push(line);
          }
          inExpr++;
        } else if (ch === ')') {
          inExpr--;
        }
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
      vm.currentFile = templatePath;

      // Create ProcessContext for sosofo execution
      const processContext = new ProcessContext(backend, vm);

      // Execute each expression (defines rules, functions, etc.)
      for (let i = 0; i < exprs.length; i++) {
        const expr = exprs[i];

        // Update compiler location for proper source tracking in instructions
        // Port from: OpenJade threads location through compilation
        compiler.currentFile = templatePath;
        compiler.currentLine = exprLineNumbers[i] || 0;

        // Also update VM location for initial context
        vm.currentLine = exprLineNumbers[i] || 0;
        vm.currentColumn = 0;  // Column tracking not yet implemented

        // Compile expression to bytecode
        const bytecode = compiler.compile(expr, env, 0, null);

        // Execute bytecode
        vm.eval(bytecode);
      }

      // After loading all definitions, automatically invoke processing
      // Port from: OpenJade jade.cxx - automatically starts root processing

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

      // For FOT backend, output the generated XML to stdout
      if (options.backend === 'fot' && backend.getOutput) {
        console.log(backend.getOutput());
      }

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
