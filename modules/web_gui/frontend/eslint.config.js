import { defineConfig } from 'eslint/config';
import reactHooks from 'eslint-plugin-react-hooks';
import globals from 'globals';
import tseslint from 'typescript-eslint';

export default defineConfig({
  files: ['**/*.{js,mjs,cjs,ts,tsx}'],
  extends: [tseslint.configs.recommended],
  plugins: {
    'react-hooks': reactHooks,
  },
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
});
