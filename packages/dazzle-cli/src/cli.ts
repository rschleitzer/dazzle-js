#!/usr/bin/env node
/**
 * Dazzle CLI - OpenJade-compatible command line interface
 *
 * Port from: OpenJade jade/jade.cxx
 */

import { Command } from 'commander';
import * as fs from 'fs';
import * as path from 'path';
import { parseXmlGrove, parse, Compiler, GlobalEnvironment, Environment, VM } from 'dazzle-core';
// TODO: Import backend when DSSSL processing is integrated
// import { createTransformBackend } from 'dazzle-backend-sgml';

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

      // Read template
      const templatePath = path.resolve(options.template);
      const templateContent = fs.readFileSync(templatePath, 'utf-8');

      // Read input XML
      const inputPath = path.resolve(inputFile);
      const inputContent = fs.readFileSync(inputPath, 'utf-8');

      // Parse input XML to grove
      const grove = parseXmlGrove(inputContent);

      // Create backend
      const outputDir = path.resolve(options.output);
      // TODO: Wire backend into DSSSL processing context
      // const backend = createTransformBackend(outputDir);

      // Parse template into S-expressions
      const exprs = parse(templateContent);

      // Create global environment and compiler
      const globals = new GlobalEnvironment();
      const compiler = new Compiler(globals);
      const env = new Environment();

      // Create VM
      const vm = new VM();

      console.log(`Template: ${templatePath}`);
      console.log(`Input: ${inputPath}`);
      console.log(`Backend: ${options.backend}`);
      console.log(`Output dir: ${outputDir}`);
      console.log(`Grove root: ${grove.root().gi()}`);
      console.log(`Parsed ${exprs.length} expression(s) from template\n`);

      // Execute each expression
      for (let i = 0; i < exprs.length; i++) {
        const expr = exprs[i];
        console.log(`Executing expression ${i + 1}...`);

        // Compile expression to bytecode
        const bytecode = compiler.compile(expr, env, 0, null);

        // Execute bytecode
        const result = vm.eval(bytecode);

        console.log(`Result: ${result.asString()?.value || result.asNumber()?.value || result.asBoolean()?.value || '(value)'}`);
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
