import { defineConfig } from 'eslint/config';
import globals from 'globals';
import tseslint from 'typescript-eslint';

export default defineConfig(
  {
    ignores: [
      '**/dist/**',
      '**/build/**',
      '**/coverage/**',
      '**/lcov-report/**',
      '**/*.spec.{cjs,js,ts,mjs}',
      '**/*.min.js',
      '**/*.bundle.js',
      '**/*.js.map',
      '**/*.d.ts',
      '**/.cache/**',
      '**/*.log',
      '**/katex.min.js',
    ],
  },
  {
    files: ['**/*.{js,mjs,cjs,ts,tsx}'],
    extends: [tseslint.configs.recommended],
    languageOptions: {
      ecmaVersion: 2021,
      sourceType: 'module',
      globals: {
        ...globals.browser,
        ...globals.node,
      },
    },
    linterOptions: {
      reportUnusedDisableDirectives: false,
    },
    rules: {
      'no-unused-vars': 'off',
      '@typescript-eslint/no-unused-vars': [
        'error',
        {
          vars: 'all',
          args: 'all',
          ignoreRestSiblings: false,
          varsIgnorePattern: '^(?:_|e|err|error)',
          argsIgnorePattern: '^(?:_|e|err|error)',
          caughtErrorsIgnorePattern: '^(?:_|e|err|error)',
        },
      ],
      'no-unused-expressions': [
        'error',
        {
          allowShortCircuit: false,
          allowTernary: false,
          allowTaggedTemplates: false,
        },
      ],
    },
  }
);
