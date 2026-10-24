%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
filename = [tempname(), '.json'];
d = dictionary(["left", "right"], [1 2]);
%=============================================================================
writedictionary(d, filename, 'PrettyPrint', false);
asserts.istrue(isfile(filename));
txt = fileread(filename);
asserts.istrue(contains(txt, '"left":1'));
asserts.istrue(contains(txt, '"right":2'));
%=============================================================================
r = readdictionary(filename);
asserts.isequal(keys(r), ["left"; "right"]);
asserts.isequal(values(r), [1; 2]);
%=============================================================================
rmfile(filename);
%=============================================================================
