---
name: nelson-usage-strings-text
audience: user
modules: string
description: Work with text in Nelson - build, split, search, replace, format, and convert between strings, character arrays, and numbers. Use when the user manipulates text or formats output.
triggers: chaine, chaîne, string, texte, text, caractere, character, char, concatener, concatenate, strcat, separer, split, strsplit, joindre, join, remplacer, replace, strrep, chercher, find, strfind, regex, regexp, format, sprintf, num2str, majuscule, upper, minuscule, lower, contains, startswith, endswith
functions: string, char, cellstr, strcat, strsplit, strjoin, strrep, strfind, regexp, regexprep, sprintf, num2str, str2double, strtrim, deblank, startsWith, endsWith, contains, split, join, replace, erase, strlength, isstrprop
---

# Strings and Text

Build, search, transform, and format text, choosing the right text type for the job.

## When to use
- Concatenating, splitting, searching, or replacing text.
- Formatting numbers into text, or parsing text into numbers.
- Pattern work with regular expressions.

## When NOT to use
- Reading/writing text FILES (use the import/export guide).

## Key functions
Types/convert: `string`, `char`, `cellstr`, `num2str`, `str2double`. Build/split: `strcat`, `strsplit`, `strjoin`, `split`, `join`. Search/replace: `strfind`, `strrep`, `replace`, `erase`, `contains`, `startsWith`, `endsWith`. Patterns: `regexp`, `regexprep`. Format: `sprintf`. Tidy: `strtrim`, `deblank`, `strlength`, `isstrprop`. (Signatures in the Verified Nelson API block.)

## Idioms & workflow
1. Choose the text type on purpose: `'...'` is a character array, `"..."` is a string; `string(...)` / `char(...)` / `cellstr(...)` convert between them.
2. Format with `sprintf('%.3f', x)` for controlled output; `num2str` for a quick conversion.
3. Prefer the string-aware predicates (`contains`, `startsWith`, `endsWith`) over manual `strfind` index tests.
4. For structured extraction use `regexp(text, pattern, 'tokens'/'match')`; use `regexprep` for pattern replacement.
5. Split then rejoin: `strjoin(strsplit(s, ','), ';')`.

## Gotchas
- Escape a quote by doubling it inside the same kind: `'it''s'`.
- `str2double` returns `NaN` on non-numeric text - check it rather than trusting the parse.
- Mixing `'...'` and `"..."` in the same call can change the result type; keep one kind per operation.

## Done when
The text is produced or parsed as requested, with the text type explicit and any numeric parse checked for `NaN`.
