%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 4-->
% Width, left/right alignment and precision must apply to the '%s' specifier,
% for char and string arguments alike. All expected values are the reference
% behaviour.
%=============================================================================
% left justify, pad to width
asserts.isequal(sprintf('%-10s|', 'abc'), 'abc       |');
%=============================================================================
% right justify (default), pad to width
asserts.isequal(sprintf('%10s|', 'abc'), '       abc|');
%=============================================================================
% precision truncates the string
asserts.isequal(sprintf('%.3s|', 'abcdefg'), 'abc|');
%=============================================================================
% width combined with precision
asserts.isequal(sprintf('%-10.3s|', 'abcdefg'), 'abc       |');
asserts.isequal(sprintf('%10.3s|', 'abcdefg'), '       abc|');
%=============================================================================
% width is a minimum: a longer string is not truncated by the width
asserts.isequal(sprintf('%5s|', 'abcdefgh'), 'abcdefgh|');
asserts.isequal(sprintf('%-5s|', 'abcdefgh'), 'abcdefgh|');
%=============================================================================
% no width: unchanged
asserts.isequal(sprintf('%s|', 'abc'), 'abc|');
%=============================================================================
% width taken from an argument with '*'
asserts.isequal(sprintf('%*s|', 8, 'abc'), '     abc|');
asserts.isequal(sprintf('%-*s|', 8, 'abc'), 'abc     |');
%=============================================================================
% string scalar arguments behave like char
asserts.isequal(sprintf('%-10s|', "abc"), 'abc       |');
asserts.isequal(sprintf('%10s|', "abc"), '       abc|');
asserts.isequal(sprintf('%.3s|', "abcdefg"), 'abc|');
%=============================================================================
% mixed with a numeric field on the same line
asserts.isequal(sprintf('[%-8s|%8s]', 'l', 'r'), '[l       |       r]');
%=============================================================================
% fprintf writes the same formatted text
str = evalc("fprintf('%-6s|', 'ab')");
asserts.isequal(str, 'ab    |');
%=============================================================================
