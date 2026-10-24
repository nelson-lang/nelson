%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function tuneHeaderTag_rewrite(filename, pattern, replacement)
  sourceBytes = tuneHeaderTag_readBytes(filename);
  eol = tuneHeaderTag_eol(sourceBytes);
  hasBom = tuneHeaderTag_hasBom(sourceBytes);
  contentBytes = sourceBytes;
  if hasBom
    contentBytes = contentBytes(4:end);
  end
  content = tuneHeaderTag_decodeUtf8(contentBytes);
  normalized = replace(content, char([13 10]), char(10));
  normalized = replace(normalized, char(13), char(10));
  lines = strsplit(normalized, char(10), 'CollapseDelimiters', false);
  lines = tuneHeaderTag_set(lines, pattern, replacement);
  updated = strjoin(lines, eol);
  if strcmp(updated, content)
    return
  end
  updatedBytes = tuneHeaderTag_encodeUtf8(updated);
  if hasBom
    updatedBytes = [uint8([239 187 191]), updatedBytes];
  end
  tuneHeaderTag_writeBytes(filename, updatedBytes);
end
%=============================================================================
function lines = tuneHeaderTag_set(lines, pattern, replacement)
  tagIndex = tuneHeaderTag_find(lines, pattern);
  if isempty(replacement) && tagIndex > 0
    lines(tagIndex) = [];
  elseif ~isempty(replacement) && tagIndex > 0
    lines{tagIndex} = replacement;
  elseif ~isempty(replacement)
    insertAfter = tuneHeaderTag_insertAfter(lines);
    lines = [lines(1:insertAfter), {replacement}, lines(insertAfter + 1:end)];
  end
end
%=============================================================================
function index = tuneHeaderTag_find(lines, pattern)
  index = 0;
  for k = 1:length(lines)
    if tuneHeaderTag_isCodeLine(lines{k})
      return
    end
    if ~isempty(regexp(lines{k}, pattern, 'once'))
      index = k;
      return
    end
  end
end
%=============================================================================
function index = tuneHeaderTag_insertAfter(lines)
  licenseIndex = tuneHeaderTag_licenseIndex(lines);
  if licenseIndex == 0
    index = 0;
    return
  end
  index = tuneHeaderTag_headerSeparator(lines, licenseIndex);
end
%=============================================================================
function index = tuneHeaderTag_licenseIndex(lines)
  index = 0;
  for k = 1:length(lines)
    if contains(lines{k}, 'LICENCE_BLOCK_END')
      index = k;
      return
    end
  end
end
%=============================================================================
function index = tuneHeaderTag_headerSeparator(lines, licenseIndex)
  index = licenseIndex;
  separatorPattern = '^[ \t]*%={5,}[ \t]*$';
  for k = licenseIndex + 1:length(lines)
    if ~isempty(regexp(lines{k}, separatorPattern, 'once'))
      index = k;
      return
    end
    if tuneHeaderTag_isCodeLine(lines{k})
      return
    end
  end
end
%=============================================================================
function tf = tuneHeaderTag_isCodeLine(line)
  trimmed = strtrim(line);
  tf = ~isempty(trimmed) && ~startsWith(trimmed, '%');
end
%=============================================================================
function tf = tuneHeaderTag_hasBom(bytes)
  tf = length(bytes) >= 3 && isequal(bytes(1:3), uint8([239 187 191]));
end
%=============================================================================
function bytes = tuneHeaderTag_readBytes(filename)
  fid = fopen(filename, 'rb');
  if fid == -1
    error('Nelson:tests_manager:cannotOpenFile', 'Unable to open file.');
  end
  cleanup = onCleanup(@() fclose(fid)); %#ok<NLS0004>
  bytes = fread(fid, Inf, 'uint8')';
  clear cleanup
end
%=============================================================================
function tuneHeaderTag_writeBytes(filename, bytes)
  fid = fopen(filename, 'wb');
  if fid == -1
    error('Nelson:tests_manager:cannotOpenFile', 'Unable to open file.');
  end
  cleanup = onCleanup(@() fclose(fid)); %#ok<NLS0004>
  count = fwrite(fid, bytes, 'uint8');
  if count ~= length(bytes)
    error('Nelson:tests_manager:cannotWriteFile', 'Unable to write file.');
  end
  clear cleanup
end
%=============================================================================
function eol = tuneHeaderTag_eol(bytes)
  eol = char(10);
  hasCrlf = length(bytes) > 1 ...
    && any(bytes(1:end - 1) == 13 & bytes(2:end) == 10);
  if hasCrlf
    eol = char([13 10]);
  elseif any(bytes == 13)
    eol = char(13);
  end
end
%=============================================================================
function text = tuneHeaderTag_decodeUtf8(bytes)
  if exist('native2unicode', 'builtin') == 5
    text = native2unicode(bytes, 'UTF-8');
  else
    text = char(bytes);
  end
end
%=============================================================================
function bytes = tuneHeaderTag_encodeUtf8(text)
  if exist('unicode2native', 'builtin') == 5
    bytes = unicode2native(text, 'UTF-8');
  else
    bytes = uint8(text);
  end
end
%=============================================================================
