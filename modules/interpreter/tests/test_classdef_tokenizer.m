%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ENGLISH IMPOSED-->
%=============================================================================
% Native classdef front-end tokenizer. __classdef_tokenize__
% turns classdef source into a flat token stream. These lock the load-bearing
% lexical decisions the recursive-descent parser will rely on: transpose vs
% char-array quoting, comment stripping, continuation folding and error
% reporting.
%=============================================================================
% Transpose after a value is an operator, not a char-array; the trailing
% comment is stripped.
t = __classdef_introspect__('tokenize', 'A = [1 2 3]'' % note');
asserts.istrue(contains(t, sprintf('op ''\n')));
asserts.isfalse(contains(t, 'note'));
%=============================================================================
% A quote after '=' opens a char array.
t = __classdef_introspect__('tokenize', 'x = ''hello''');
asserts.istrue(contains(t, sprintf('char ''hello''\n')));
%=============================================================================
% A double-quoted string is a string literal.
t = __classdef_introspect__('tokenize', 'x = "hi"');
asserts.istrue(contains(t, sprintf('string "hi"\n')));
%=============================================================================
% A newline inside open brackets is an implicit continuation: no statement
% break between the matrix rows.
t = __classdef_introspect__('tokenize', sprintf('M = [1 2\n3 4]'));
asserts.isfalse(contains(t, sprintf('number 2\nnewline')));
%=============================================================================
% Block keywords are ordinary identifiers (they are also builtin functions);
% the parser recognizes them contextually.
t = __classdef_introspect__('tokenize', 'classdef Point < handle');
asserts.istrue(contains(t, sprintf('ident classdef\n')));
asserts.istrue(contains(t, sprintf('ident Point\n')));
asserts.istrue(contains(t, sprintf('less <\n')));
%=============================================================================
% An unterminated char array is a lexical error.
t = __classdef_introspect__('tokenize', 'x = ''oops');
asserts.istrue(startsWith(t, 'error='));
%=============================================================================
% Every classdef fixture tokenizes without a lexical error.
classdef_dir = [modulepath('interpreter', 'tests'), '/classdef'];
files = dir([classdef_dir, '/*.m']);
checked = 0;
for k = 1:numel(files)
  source = fileread([files(k).folder, '/', files(k).name]);
  if ~strcmp(parsestring(source), 'classdef')
    continue;
  end
  tokens = __classdef_introspect__('tokenize', source);
  asserts.isfalse(startsWith(tokens, 'error='));
  asserts.istrue(endsWith(tokens, sprintf('eof\n')));
  checked = checked + 1;
end
asserts.istrue(checked > 10);
%=============================================================================
