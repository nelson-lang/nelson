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
c = onCleanup(@() rmdir(probe_dir, 's')); %#ok<NASGU>
LF = 10;
CR = 13;
contents = {
  'lf_final', [double('a'), LF, double('b'), LF];
  'mixed', [double('a'), CR, LF, CR, LF, double('  b '), 9, CR, double('c')];
  'empty', [];
  'lf_only', LF;
  'blank_end', [double('a'), LF, LF];
  'cr_crlf', [double('a'), CR, CR, LF, double('b')];
  'spaces', [double('a'), LF, double('  '), LF, double('b')];
  'lf_blank_end', [double('a'), LF, double('b'), LF, LF];
  'crlf_final', [double('a'), CR, LF];
  'bom', [239, 187, 191, double('ab'), LF, double('c')];
  'utf8', [195, 169, 104, LF];
  'latin1', [233, 104, LF];
  'many_blank', [double('a'), LF, LF, LF, double('b'), LF];
  'blank_trim', [double(' '), 9, LF]};
F = struct();
for k = 1:size(contents, 1)
  F.(contents{k, 1}) = fullfile(probe_dir, [contents{k, 1}, '.txt']);
  fid = fopen(F.(contents{k, 1}), 'wb');
  fwrite(fid, uint8(contents{k, 2}));
  fclose(fid);
end
%=============================================================================
% default: column string array, a final line terminator gives a final empty line
S = readlines(F.lf_final);
asserts.isequal(S, ["a"; "b"; ""]);
S = readlines(string(F.lf_final));
asserts.isequal(S, ["a"; "b"; ""]);
S = readlines(F.mixed);
asserts.isequal(S, ["a"; ""; string(['  b ', char(9)]); "c"]);
S = readlines(F.cr_crlf);
asserts.isequal(S, ["a"; ""; "b"]);
S = readlines(F.lf_only);
asserts.isequal(S, [""; ""]);
S = readlines(F.blank_end);
asserts.isequal(S, ["a"; ""; ""]);
%=============================================================================
% empty file
S = readlines(F.empty);
asserts.isequal(class(S), 'string');
asserts.isequal(S, "");
S = readlines(F.empty, 'EmptyLineRule', 'skip');
asserts.isequal(size(S), [0 1]);
asserts.isequal(class(S), 'string');
S = readlines(F.empty, 'EmptyLineRule', 'error');
asserts.isequal(size(S), [0 1]);
%=============================================================================
% EmptyLineRule
S = readlines(F.mixed, 'EmptyLineRule', 'skip');
asserts.isequal(S, ["a"; string(['  b ', char(9)]); "c"]);
S = readlines(F.blank_end, 'EmptyLineRule', 'skip');
asserts.isequal(S, "a");
S = readlines(F.spaces, 'EmptyLineRule', 'skip');
asserts.isequal(S, ["a"; "b"]);
S = readlines(F.many_blank, 'EmptyLineRule', 'skip');
asserts.isequal(S, ["a"; "b"]);
S = readlines(F.lf_final, 'EmptyLineRule', 'error');
asserts.isequal(S, ["a"; "b"]);
S = readlines(F.crlf_final, 'EmptyLineRule', 'error');
asserts.isequal(S, "a");
asserts.checkerror('readlines(F.mixed, ''EmptyLineRule'', ''error'')', 'Unexpected empty line detected on row 2.');
asserts.checkerror('readlines(F.mixed, ''EmptyLineRule'', ''error'')', 'Unexpected empty line detected on row 2.', 'Nelson:readlines:emptyLine');
asserts.checkerror('readlines(F.lf_only, ''EmptyLineRule'', ''error'')', 'Unexpected empty line detected on row 1.');
asserts.checkerror('readlines(F.blank_end, ''EmptyLineRule'', ''error'')', 'Unexpected empty line detected on row 2.');
asserts.checkerror('readlines(F.lf_blank_end, ''EmptyLineRule'', ''error'')', 'Unexpected empty line detected on row 3.');
asserts.checkerror('readlines(F.spaces, ''EmptyLineRule'', ''error'')', 'Unexpected empty line detected on row 2.');
asserts.checkerror('readlines(F.many_blank, ''EmptyLineRule'', ''error'')', 'Unexpected empty line detected on row 2.');
asserts.checkerror('readlines(F.blank_trim, ''WhitespaceRule'', ''trim'', ''EmptyLineRule'', ''error'')', 'Unexpected empty line detected on row 1.');
%=============================================================================
% WhitespaceRule and Whitespace
S = readlines(F.mixed, 'WhitespaceRule', 'trim');
asserts.isequal(S, ["a"; ""; "b"; "c"]);
S = readlines(F.mixed, 'WhitespaceRule', 'trimleading');
asserts.isequal(S, ["a"; ""; string(['b ', char(9)]); "c"]);
S = readlines(F.mixed, 'WhitespaceRule', 'trimtrailing');
asserts.isequal(S, ["a"; ""; "  b"; "c"]);
S = readlines(F.mixed, 'Whitespace', ' ', 'WhitespaceRule', 'trim');
asserts.isequal(S, ["a"; ""; string(['b ', char(9)]); "c"]);
S = readlines(F.spaces, 'Whitespace', '', 'WhitespaceRule', 'trim');
asserts.isequal(S, ["a"; "  "; "b"]);
S = readlines(F.spaces, 'Whitespace', "ab ", 'WhitespaceRule', 'trim');
asserts.isequal(S, [""; ""; ""]);
S = readlines(F.spaces, 'Whitespace', '\t');
asserts.isequal(S, ["a"; "  "; "b"]);
S = readlines(F.spaces, 'EmptyLineRule', 'skip', 'WhitespaceRule', 'trim');
asserts.isequal(S, ["a"; "b"]);
S = readlines(F.blank_trim, 'WhitespaceRule', 'trim');
asserts.isequal(S, [""; ""]);
S = readlines(F.mixed, 'WHITESPACERULE', 'TRIM');
asserts.isequal(S, ["a"; ""; "b"; "c"]);
%=============================================================================
% LineEnding (escape sequences are interpreted)
S = readlines(F.mixed, 'LineEnding', '\n');
asserts.isequal(S, [string(['a', char(13)]); string(char(13)); string(['  b ', char([9 13]), 'c'])]);
S = readlines(F.mixed, 'LineEnding', '\r\n');
asserts.isequal(S, ["a"; ""; string(['  b ', char([9 13]), 'c'])]);
S = readlines(F.mixed, 'LineEnding', "x");
asserts.isequal(strlength(S), 12);
S = readlines(F.spaces, 'LineEnding', '');
asserts.isequal(size(S), [1 1]);
S = readlines(F.cr_crlf, 'LineEnding', {'\n', '\r'});
asserts.isequal(S, ["a"; ""; ""; "b"]);
S = readlines(F.cr_crlf, 'LineEnding', "\r\n");
asserts.isequal(S, [string(['a', char(13)]); "b"]);
S = readlines(F.crlf_final, 'LineEnding', '\n');
asserts.isequal(S, [string(['a', char(13)]); ""]);
S = readlines(F.crlf_final, 'LineEnding', ["\n", "\r"]);
asserts.isequal(S, ["a"; ""; ""]);
%=============================================================================
% byte order mark and Encoding
S = readlines(F.bom);
asserts.isequal(S, ["ab"; "c"]);
S = readlines(F.utf8);
asserts.isequal(double(char(S(1))), [233 104]);
S = readlines(F.utf8, 'Encoding', 'UTF-8');
asserts.isequal(double(char(S(1))), [233 104]);
if ~iswasm()
S = readlines(F.utf8, 'Encoding', 'ISO-8859-1');
asserts.isequal(double(char(S(1))), [195 169 104]);
S = readlines(F.latin1, 'Encoding', 'ISO-8859-1');
asserts.isequal(double(char(S(1))), [233 104]);
end
S = readlines(F.lf_final, 'Encoding', 'system');
asserts.isequal(S, ["a"; "b"; ""]);
%=============================================================================
% errors
asserts.checkerror('readlines(fullfile(probe_dir, ''nofile.txt''))', sprintf('Unable to find or open ''%s''. Check the path and filename or file permissions.', fullfile(probe_dir, 'nofile.txt')));
asserts.checkerror('readlines(fullfile(probe_dir, ''nofile.txt''))', sprintf('Unable to find or open ''%s''. Check the path and filename or file permissions.', fullfile(probe_dir, 'nofile.txt')), 'Nelson:readlines:fileNotFound');
asserts.checkerror('readlines(probe_dir)', 'Expected a single file. Folders are not supported.', 'Nelson:readlines:folderNotSupported');
asserts.checkerror('readlines(3)', '"filename" must be a string scalar or character vector.', 'Nelson:readlines:invalidFilename');
asserts.checkerror('readlines(F.lf_final, ''EmptyLineRule'')', 'All parameters must have an associated value.', 'Nelson:readlines:oddNameValuePairs');
asserts.checkerror('readlines(F.lf_final, ''Foo'', 1)', 'Unknown Parameter ''Foo''.', 'Nelson:readlines:unknownParameter');
asserts.checkerror('readlines(F.lf_final, ''EmptyLineRule'', ''bad'')', 'EmptyLineRule must be one of these values: ''read'', ''skip'', ''error''.', 'Nelson:readlines:invalidOptionValue');
asserts.checkerror('readlines(F.lf_final, ''WhitespaceRule'', ''bad'')', 'WhitespaceRule must be one of these values: ''preserve'', ''trim'', ''trimleading'', ''trimtrailing''.', 'Nelson:readlines:invalidOptionValue');
asserts.checkerror('readlines(F.lf_final, ''Encoding'', ''bad'')', 'The encoding ''bad'' is not valid.', 'Nelson:readlines:invalidEncoding');
asserts.checkerror('readlines(F.lf_final, ''Encoding'', 1)', 'Encoding must be a string scalar or character vector.', 'Nelson:readlines:encodingTypeExpected');
asserts.checkerror('readlines(F.lf_final, ''LineEnding'', 1)', 'LineEnding must be a string array, character vector, or cell array of character vectors.', 'Nelson:readlines:invalidLineEnding');
asserts.checkerror('readlines()', _('Not enough input arguments.'));
asserts.checkerror('[a, b] = readlines(F.lf_final)', _('Too many output arguments.'));
%=============================================================================
