%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% a character matrix loses its trailing all-blank columns
asserts.isequal(deblank(['a  '; 'bc ']), ['a '; 'bc']);
asserts.isequal(deblank(['ab '; 'cd ']), ['ab'; 'cd']);
asserts.isequal(deblank(['a  '; 'b  ']), ['a'; 'b']);
asserts.isequal(deblank(['  a'; '  b']), ['  a'; '  b']);
asserts.isequal(deblank(['a  '; '  b']), ['a  '; '  b']);
asserts.isequal(deblank(char([97 9 10; 98 32 13])), ['a'; 'b']);
asserts.isequal(deblank(char([97 0 0; 98 0 32])), ['a'; 'b']);
asserts.isequal(size(deblank(char(zeros(2, 3) + 32))), [0 0]);
asserts.isequal(size(deblank('   ')), [0 0]);
asserts.isequal(deblank({'a  ', ['b  '; 'cc ']}), {'a', ['b '; 'cc']});
%=============================================================================
% trailing NUL characters and Unicode space separators are removed too
asserts.isequal(deblank(sprintf('a\0')), 'a');
asserts.isequal(deblank(sprintf('a\0b  ')), ['a', char(0), 'b']);
asserts.isequal(deblank(char([97 0 32])), 'a');
asserts.isequal(deblank(char([97 8192])), 'a');
asserts.isequal(deblank(char([97 12288])), 'a');
asserts.isequal(deblank(char([97 160])), char([97 160]));
%=============================================================================
% values that are not text are returned unchanged
asserts.isequal(deblank(5), 5);
asserts.isequal(deblank([5 6]), [5 6]);
asserts.isequal(deblank(int8(5)), int8(5));
asserts.isequal(deblank({'a  ', 5}), {'a', 5});
asserts.isequal(deblank({'a  ', {'b'}}), {'a', {'b'}});
asserts.isequal(deblank({'a  ', "b "}), {'a', "b"});
asserts.isequal(deblank({'a  ', []}), {'a', []});
asserts.isequal(size(deblank(cell(0, 3))), [0 3]);
%=============================================================================
