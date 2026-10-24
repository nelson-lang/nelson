%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function typst = markdown_to_typst(markdown, label, helpers)
  % Converts the hand-written markdown pages of the manual (home page,
  % getting started, changelogs, licenses) to Typst markup.
  %
  % Supported: ATX headings, paragraphs, bullet and numbered lists (nested by
  % indentation), fenced code blocks, pipe tables, inline code, bold, italic,
  % links, <img> tags; <link> and <script> tags are dropped. The first heading
  % receives the label <label> so that pages can be cross-referenced with
  % nlink. Every other character is escaped for Typst.
  % helpers: relative path of nelson_help.typ from the generated file.
  if nargin < 3
    helpers = 'nelson_help.typ';
  end
  lines = regexp(strrep(markdown, char([13 10]), char(10)), char(10), 'split');
  out = {['#import "', helpers, '": *']; ''};
  in_fence = false;
  fence_lang = '';
  fence_lines = {};
  table_rows = {};
  first_heading = true;
  heading_shift = 0;
  k = 1;
  while k <= numel(lines)
    line = lines{k};
    k = k + 1;
    if in_fence
      if startsWith(strtrim(line), '```')
        out{end + 1} = ['``````', fence_lang];
        out = [out; fence_lines(:)];
        out{end + 1} = '``````';
        out{end + 1} = '';
        in_fence = false;
        fence_lines = {};
      else
        fence_lines{end + 1} = line;
      end
      continue;
    end
    trimmed = strtrim(line);
    % pipe tables are collected until a non table line
    if startsWith(trimmed, '|')
      table_rows{end + 1} = trimmed;
      if k > numel(lines) || ~startsWith(strtrim(lines{k}), '|')
        out{end + 1} = tableToTypst(table_rows);
        out{end + 1} = '';
        table_rows = {};
      end
      continue;
    end
    if startsWith(trimmed, '```')
      in_fence = true;
      fence_lang = mapLanguage(strtrim(trimmed(4:end)));
      continue;
    end
    % html only elements
    if startsWith(trimmed, '<link') || startsWith(trimmed, '<script')
      continue;
    end
    img = regexp(trimmed, '<img\s[^>]*src="([^"]+)"', 'tokens', 'once');
    if ~isempty(img)
      out{end + 1} = ['#align(center)[#image("', img{1}, '")]'];
      out{end + 1} = '';
      continue;
    end
    % headings
    heading = regexp(trimmed, '^(#{1,6})\s+(.*?)\s*#*$', 'tokens', 'once');
    if ~isempty(heading)
      level = numel(heading{1});
      % links are reduced to their text: a contextual nlink inside a heading
      % would leave the PDF bookmark empty
      text = inlineToTypst(regexprep(heading{2}, '\[([^\]]+)\]\([^)]*\)', '$1'));
      if first_heading
        % the first heading of a page is its chapter title, whatever its
        % markdown level; the following headings keep their relative depth
        % but never become chapters themselves (changelog versions, license
        % sections are pages of the chapter)
        heading_shift = level - 1;
        level = 1;
        text = [text, ' <', label, '>'];
        first_heading = false;
      else
        level = min(max(level - heading_shift, 2), 6);
      end
      out{end + 1} = [repmat('=', 1, level), ' ', text];
      out{end + 1} = '';
      continue;
    end
    % list items keep their indentation (nesting) and marker
    item = regexp(line, '^(\s*)([-*+]|\d+[.)])\s+(.*)$', 'tokens', 'once');
    if ~isempty(item)
      indent = item{1};
      marker = item{2};
      if any(marker == '-') || any(marker == '*') || any(marker == '+')
        marker = '-';
      else
        marker = '+';
      end
      out{end + 1} = [indent, marker, ' ', inlineToTypst(item{3})];
      continue;
    end
    if isempty(trimmed)
      out{end + 1} = '';
      continue;
    end
    % continuation lines of list items keep their indentation
    indent = regexp(line, '^\s*', 'match', 'once');
    out{end + 1} = [indent, inlineToTypst(trimmed)];
  end
  if in_fence
    out{end + 1} = ['``````', fence_lang];
    out = [out; fence_lines(:)];
    out{end + 1} = '``````';
  end
  typst = strjoin(out, char(10));
end
%=============================================================================
function lang = mapLanguage(lang)
  switch lower(lang)
    case {'nelson', 'm', 'octave'}
      lang = 'matlab';
    case {'sh', 'shell', 'console'}
      lang = 'bash';
    otherwise
  end
end
%=============================================================================
function typst = tableToTypst(rows)
  cells = {};
  for r = 1:numel(rows)
    row = rows{r};
    if r == 2 && ~isempty(regexp(row, '^\|?\s*:?-{2,}', 'once'))
      continue;
    end
    row = regexprep(row, '^\|', '');
    row = regexprep(row, '\|\s*$', '');
    parts = strsplit(row, '|');
    cells{end + 1} = cellfun(@(c) inlineToTypst(strtrim(c)), parts, 'UniformOutput', false);
  end
  if isempty(cells)
    typst = '';
    return;
  end
  ncols = max(cellfun(@numel, cells));
  lines = {['#table(', char(10), '  columns: ', num2str(ncols), ',']};
  for r = 1:numel(cells)
    row = cells{r};
    row(end + 1:ncols) = {''};
    items = strjoin(cellfun(@(c) ['[', c, ']'], row, 'UniformOutput', false), ', ');
    if r == 1
      lines{end + 1} = ['  table.header(', items, '),'];
    else
      lines{end + 1} = ['  ', items, ','];
    end
  end
  lines{end + 1} = ')';
  typst = strjoin(lines, char(10));
end
%=============================================================================
function typst = inlineToTypst(text)
  % Inline constructs are replaced by placeholders, the remaining text is
  % escaped, then the placeholders are substituted by their Typst form.
  tokens = {};
  % inline code (highest precedence)
  [text, tokens] = capture(text, '`([^`]+)`', @(t) ['#raw(', typstString(t{1}), ');'], tokens);
  % links [text](url) and bare <https://...>
  [text, tokens] = capture(text, '\[([^\]]+)\]\(([^)\s]+)\)', @linkToTypst, tokens);
  [text, tokens] = capture(text, '<(https?://[^>\s]+)>', @(t) ['#link(', typstString(t{1}), ');'], tokens);
  % bold then italic
  [text, tokens] = capture(text, '\*\*([^*]+)\*\*', @(t) ['#strong[', escapeText(t{1}), '];'], tokens);
  [text, tokens] = capture(text, '(?<![\w*])\*([^*\s][^*]*?)\*(?![\w*])', @(t) ['#emph[', escapeText(t{1}), '];'], tokens);
  [text, tokens] = capture(text, '(?<!\w)_([^_\s][^_]*?)_(?!\w)', @(t) ['#emph[', escapeText(t{1}), '];'], tokens);
  typst = escapeText(text);
  for n = numel(tokens):-1:1
    typst = strrep(typst, placeholder(n), tokens{n});
  end
end
%=============================================================================
function [text, tokens] = capture(text, pattern, render, tokens)
  while true
    [tok, match] = regexp(text, pattern, 'tokens', 'match', 'once');
    if isempty(match)
      break;
    end
    tokens{end + 1} = render(tok);
    text = strrep(text, match, placeholder(numel(tokens)));
  end
end
%=============================================================================
function p = placeholder(n)
  % U+E000.. private use characters never appear in the sources
  p = [char(57344), num2str(n), char(57345)];
end
%=============================================================================
function typst = linkToTypst(tok)
  text = tok{1};
  url = tok{2};
  if startsWith(url, 'http://') || startsWith(url, 'https://')
    typst = ['#link(', typstString(url), ')[', inlineToTypst(text), '];'];
  elseif endsWith(lower(url), '.md')
    [~, name] = fileparts(url);
    typst = ['#nlink(<main:', regexprep(name, '[^A-Za-z0-9_.-]', '-'), '>)[', inlineToTypst(text), '];'];
  else
    typst = inlineToTypst(text);
  end
end
%=============================================================================
function s = typstString(text)
  s = ['"', strrep(strrep(text, '\', '\\'), '"', '\"'), '"'];
end
%=============================================================================
function text = escapeText(text)
  text = strrep(text, '\', '\\');
  for c = '#$*_`<>@[]~='
    text = strrep(text, c, ['\', c]);
  end
  text = strrep(text, '//', '\/\/');
end
%=============================================================================
