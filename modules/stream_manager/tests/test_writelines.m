%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
probe_dir = tempname();
mkdir(probe_dir);
c = onCleanup(@() rmdir(probe_dir, 's')); %#ok<NLS0004>
f = fullfile(probe_dir, 'out.txt');
if ispc()
  NL = [13 10];
else
  NL = 10;
end
CRLF = [13 10];
%=============================================================================
% text inputs: lines are taken in column order, the default rule ends the last line
writelines(["a"; "b"], f);
asserts.isequal(readBytes(f), [97, NL, 98, NL]);
writelines(["a", "b"], f);
asserts.isequal(readBytes(f), [97, NL, 98, NL]);
writelines(["a", "b"; "c", "d"], f);
asserts.isequal(readBytes(f), [97, NL, 99, NL, 98, NL, 100, NL]);
writelines("abc", f);
asserts.isequal(readBytes(f), [97 98 99, NL]);
writelines('abc', f);
asserts.isequal(readBytes(f), [97 98 99, NL]);
writelines({'a'; 'b'}, f);
asserts.isequal(readBytes(f), [97, NL, 98, NL]);
writelines({'a', 'bb'}, f);
asserts.isequal(readBytes(f), [97, NL, 98 98, NL]);
writelines({'a', 'b'; 'c', 'd'}, f);
asserts.isequal(readBytes(f), [97, NL, 99, NL, 98, NL, 100, NL]);
writelines(["a"; string(NaN); "c"], f);
asserts.isequal(readBytes(f), [97, NL, NL, 99, NL]);
writelines({'a'; ''}, f);
asserts.isequal(readBytes(f), [97, NL]);
%=============================================================================
% empty inputs
writelines(string([]), f);
asserts.isequal(readBytes(f), NL);
writelines("", f);
asserts.isequal(readBytes(f), NL);
writelines('', f);
asserts.isequal(readBytes(f), NL);
writelines({}, f);
asserts.isequal(readBytes(f), NL);
%=============================================================================
% text is written as is: no escape and no line ending conversion
writelines(['a', char(10), 'b'], f);
asserts.isequal(readBytes(f), [97 10 98, NL]);
writelines('\n x', f);
asserts.isequal(readBytes(f), [double('\n x'), NL]);
writelines(string(['a', char([13 10])]), f);
asserts.isequal(readBytes(f), [97 13 10]);
%=============================================================================
% UTF-8 by default, without byte order mark
writelines(string([char(233), char(8364)]), f);
asserts.isequal(readBytes(f), [195 169 226 130 172, NL]);
%=============================================================================
% LineEnding (escape sequences are interpreted)
writelines(["a"; "b"], f, 'LineEnding', '\r\n');
asserts.isequal(readBytes(f), [97 13 10 98 13 10]);
writelines(["a"; "b"], f, 'LineEnding', char(10));
asserts.isequal(readBytes(f), [97 10 98 10]);
writelines(["a"; "b"], f, 'LineEnding', 'XY');
asserts.isequal(readBytes(f), [97 88 89 98 88 89]);
writelines(["a"; "b"], f, 'LineEnding', ";");
asserts.isequal(readBytes(f), [97 59 98 59]);
writelines(["a"; "b"], f, 'LineEnding', '\t');
asserts.isequal(readBytes(f), [97 9 98 9]);
writelines("x", f, 'LineEnding', '\\');
asserts.isequal(readBytes(f), [120 92]);
writelines("x", f, 'LineEnding', '\q');
asserts.isequal(readBytes(f), [120 92 113]);
writelines("x", f, 'Line', '\n');
asserts.isequal(readBytes(f), [120 10]);
writelines("x", f, 'lineending', '\r\n');
asserts.isequal(readBytes(f), [120 13 10]);
writelines(string(['a', char(10)]), f, 'LineEnding', '\n');
asserts.isequal(readBytes(f), [97 10]);
%=============================================================================
% TrailingLineEndingRule
writelines(["a"; "b"], f, 'TrailingLineEndingRule', 'never');
asserts.isequal(readBytes(f), [97, NL, 98]);
writelines(["a"; "b"], f, 'TrailingLineEndingRule', 'ALW');
asserts.isequal(readBytes(f), [97, NL, 98, NL]);
writelines(["a"; ""], f, 'TrailingLineEndingRule', 'always');
asserts.isequal(readBytes(f), [97, NL, NL]);
writelines("", f, 'TrailingLineEndingRule', 'never');
asserts.isequal(readBytes(f), zeros(1, 0));
writelines(string([]), f, 'TrailingLineEndingRule', 'never');
asserts.isequal(readBytes(f), zeros(1, 0));
writelines(string([]), f, 'TrailingLineEndingRule', 'always');
asserts.isequal(readBytes(f), NL);
writelines(["a"; ""; ""], f, 'LineEnding', '\r\n');
asserts.isequal(readBytes(f), [97 13 10 13 10]);
writelines(["a"; "b"], f, 'LineEnding', '\r\n', 'TrailingLineEndingRule', 'never');
asserts.isequal(readBytes(f), [97 13 10 98]);
%=============================================================================
% WriteMode append: added lines start on a new line of a non-empty file
pre = {[], double('OLD'), [double('OLD'), 10], [double('OLD'), CRLF], [239 187 191]};
for k = 1:numel(pre)
  writeBytes(f, pre{k});
  writelines(["x"; "y"], f, 'WriteMode', 'append', 'LineEnding', '\r\n');
  if isempty(pre{k})
    prefix = [];
  elseif isequal(pre{k}(max(end - 1, 1):end), CRLF)
    prefix = pre{k};
  else
    prefix = [pre{k}, CRLF];
  end
  asserts.isequal(readBytes(f), [prefix, 120 13 10 121 13 10]);
end
writeBytes(f, double('OLD'));
writelines(["x"; "y"], f, 'WriteMode', 'append', 'LineEnding', '\r\n', 'TrailingLineEndingRule', 'never');
asserts.isequal(readBytes(f), [double('OLD'), 13 10 120 13 10 121]);
writeBytes(f, double('OLD'));
writelines(string([]), f, 'WriteMode', 'append', 'LineEnding', '\r\n');
asserts.isequal(readBytes(f), [double('OLD'), 13 10]);
writeBytes(f, double('OLD'));
writelines(string([]), f, 'WriteMode', 'append', 'LineEnding', '\r\n', 'TrailingLineEndingRule', 'always');
asserts.isequal(readBytes(f), [double('OLD'), 13 10 13 10]);
writeBytes(f, double('OLD'));
writelines(string([]), f, 'WriteMode', 'append', 'TrailingLineEndingRule', 'never');
asserts.isequal(readBytes(f), double('OLD'));
writeBytes(f, []);
writelines(string([]), f, 'WriteMode', 'append', 'LineEnding', '\r\n');
asserts.isequal(readBytes(f), [13 10]);
writeBytes(f, double('OLD;'));
writelines("x", f, 'WriteMode', 'app', 'LineEnding', ';');
asserts.isequal(readBytes(f), double('OLD;x;'));
f_new = fullfile(probe_dir, 'new.txt');
writelines("z", f_new, 'WriteMode', 'APPEND');
asserts.isequal(readBytes(f_new), [122, NL]);
writeBytes(f, double('OLD'));
writelines("x", f, 'WriteMode', 'overwrite');
asserts.isequal(readBytes(f), [120, NL]);
%=============================================================================
% Encoding
writelines(string(char(233)), f, 'Encoding', 'UTF-8');
asserts.isequal(readBytes(f), [195 169, NL]);
writelines(string(char(233)), f, 'Encoding', 'utf8');
asserts.isequal(readBytes(f), [195 169, NL]);
writelines(string(char(233)), f, 'Encoding', '');
asserts.isequal(readBytes(f), [195 169, NL]);
if ~iswasm()
  writelines(string(char(233)), f, 'Encoding', 'ISO-8859-1');
  asserts.isequal(readBytes(f), [233, NL]);
  writelines(string(char(8364)), f, 'Encoding', 'ISO-8859-1');
  asserts.isequal(readBytes(f), [26, NL]);
  writelines("a", f, 'Encoding', 'Shift_JIS');
  asserts.isequal(readBytes(f), [97, NL]);
  writeBytes(f, [195 169 10]);
  writelines(string(char(233)), f, 'WriteMode', 'append', 'Encoding', 'ISO-8859-1', 'LineEnding', '\r\n');
  asserts.isequal(readBytes(f), [195 169 10 13 10 233 13 10]);
end
%=============================================================================
% round trip with readlines
writelines(["a"; ""; "b"], f);
asserts.isequal(readlines(f), ["a"; ""; "b"; ""]);
writelines(["a"; ""; "b"], f, 'TrailingLineEndingRule', 'never');
asserts.isequal(readlines(f), ["a"; ""; "b"]);
%=============================================================================
% relative file name
current = pwd();
cd(probe_dir);
writelines("rel", 'rel.txt');
cd(current);
asserts.isequal(readBytes(fullfile(probe_dir, 'rel.txt')), [114 101 108, NL]);
%=============================================================================
% errors
msg = ['Invalid argument at position 1. Value must be a character vector, string array, ', ...
  'or cell array of character vectors.'];
asserts.checkerror('writelines([''ab''; ''cd''], f)', msg, 'Nelson:writelines:invalidText');
asserts.checkerror('writelines({''a'', 1}, f)', msg, 'Nelson:writelines:invalidText');
asserts.checkerror('writelines({"a"}, f)', msg, 'Nelson:writelines:invalidText');
asserts.checkerror('writelines(char(zeros(0, 3)), f)', msg, 'Nelson:writelines:invalidText');
asserts.checkerror('writelines([1 2 3], f)', msg, 'Nelson:writelines:invalidText');
asserts.checkerror('writelines(true, f)', msg, 'Nelson:writelines:invalidText');
msg = 'Invalid argument at position 2. Value must be a nonempty character vector or string scalar.';
asserts.checkerror('writelines("a", 3)', msg, 'Nelson:writelines:invalidFilename');
asserts.checkerror('writelines("a", ["a", "b"])', msg, 'Nelson:writelines:invalidFilename');
asserts.checkerror('writelines("a", "")', msg, 'Nelson:writelines:invalidFilename');
msg = ['File could not be opened for writing. Possible causes include invalid file name, ', ...
  'insufficient permissions, and insufficient disk space.'];
asserts.checkerror('writelines("a", probe_dir)', msg, 'Nelson:writelines:openFailed');
asserts.checkerror('writelines("a", fullfile(probe_dir, ''nodir'', ''x.txt''))', msg, 'Nelson:writelines:openFailed');
asserts.checkerror('writelines("a", f, ''LineEnding'', '''')', ...
  'LineEnding must be a nonempty character vector or string scalar.', 'Nelson:writelines:invalidLineEnding');
asserts.checkerror('writelines("a", f, ''LineEnding'', {''\n''})', ...
  'LineEnding must be a nonempty character vector or string scalar.', 'Nelson:writelines:invalidLineEnding');
asserts.checkerror('writelines("a", f, ''LineEnding'', 1)', ...
  'LineEnding must be a nonempty character vector or string scalar.', 'Nelson:writelines:invalidLineEnding');
asserts.checkerror('writelines("a", f, ''Encoding'', ''system'')', 'The encoding ''system'' is not valid.', ...
  'Nelson:writelines:invalidEncoding');
asserts.checkerror('writelines("a", f, ''Encoding'', ''bad'')', 'The encoding ''bad'' is not valid.', ...
  'Nelson:writelines:invalidEncoding');
asserts.checkerror('writelines("a", f, ''Encoding'', 1)', 'Encoding must be a string scalar or character vector.', ...
  'Nelson:writelines:encodingTypeExpected');
asserts.checkerror('writelines("a", f, ''WriteMode'', ''bad'')', ...
  'WriteMode must be one of these values: ''overwrite'', ''append''.', 'Nelson:writelines:invalidOptionValue');
asserts.checkerror('writelines("a", f, ''TrailingLineEndingRule'', ''bad'')', ...
  'TrailingLineEndingRule must be one of these values: ''auto'', ''always'', ''never''.', ...
  'Nelson:writelines:invalidOptionValue');
asserts.checkerror('writelines("a", f, ''WriteMode'')', 'All parameters must have an associated value.', ...
  'Nelson:writelines:oddNameValuePairs');
asserts.checkerror('writelines("a", f, ''Foo'', 1)', 'Unknown Parameter ''Foo''.', ...
  'Nelson:writelines:unknownParameter');
asserts.checkerror('writelines("a", f, 1, 1)', 'Parameter name must be a string scalar or character vector.', ...
  'Nelson:writelines:invalidParameterName');
asserts.checkerror('writelines()', _('Not enough input arguments.'));
asserts.checkerror('writelines("a")', _('Not enough input arguments.'));
asserts.checkerror('r = writelines("a", f)', _('Too many output arguments.'));
%=============================================================================
function b = readBytes(filename)
  fid = fopen(filename, 'rb');
  b = double(fread(fid, Inf, 'uint8'));
  fclose(fid);
  b = reshape(b, 1, []);
end
%=============================================================================
function writeBytes(filename, b)
  fid = fopen(filename, 'wb');
  fwrite(fid, uint8(b));
  fclose(fid);
end
%=============================================================================
