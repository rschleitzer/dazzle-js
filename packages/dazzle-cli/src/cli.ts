#!/usr/bin/env node
/**
 * Dazzle CLI - OpenJade-compatible command line interface
 *
 * Port from: OpenJade jade/jade.cxx
 */

import { Command } from 'commander';
import * as fs from 'fs';
import * as path from 'path';
import { parseXmlGrove } from 'dazzle-core';
import { createTransformBackend } from 'dazzle-backend-sgml';

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
      // TODO: Parse template content when interpreter is ready
      // const templateContent = fs.readFileSync(templatePath, 'utf-8');

      // Read input XML
      const inputPath = path.resolve(inputFile);
      const inputContent = fs.readFileSync(inputPath, 'utf-8');

      // Parse input XML to grove
      const grove = parseXmlGrove(inputContent);

      // Create backend
      const outputDir = path.resolve(options.output);
      const backend = createTransformBackend(outputDir);

      // TODO: Parse template, create interpreter, execute
      console.log('Template:', templatePath);
      console.log('Input:', inputPath);
      console.log('Backend:', options.backend);
      console.log('Output dir:', outputDir);
      console.log('Grove root:', grove.root().gi());
      console.log('Backend ready:', backend !== null);

      console.log('\nNote: Full interpreter integration coming soon!');
      console.log('Currently demonstrating:');
      console.log('  ✓ Command-line argument parsing');
      console.log('  ✓ XML parsing with libxmljs2');
      console.log('  ✓ Grove creation');
      console.log('  ✓ Backend instantiation');

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
