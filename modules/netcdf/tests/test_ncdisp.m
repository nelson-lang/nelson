%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--SEQUENTIAL TEST REQUIRED-->
asserts.isequal(nargin('ncdisp'), -1);
asserts.isequal(nargout('ncdisp'), -1);
ok = false; try; ncdisp(); catch; ok = true; end; asserts.istrue(ok);
filename = [tempname(), '.nc'];
nccreate(filename, 'x', 'Dimensions', {'n', 2});
ncwriteatt(filename, '/', 'title', 'display test');
ncwriteatt(filename, 'x', 'units', 'meter');
txt = evalc('ncdisp(filename)');
asserts.istrue(contains(txt, 'Source:'));
asserts.istrue(contains(txt, 'Format:'));
asserts.istrue(contains(txt, 'Dimensions:'));
asserts.istrue(contains(txt, 'Variables:'));
asserts.istrue(contains(txt, 'title'));
asserts.istrue(contains(txt, 'units'));
txt = evalc('ncdisp(filename, ''/'')');
asserts.istrue(contains(txt, 'Variables:'));
ok = false; try; ncdisp([tempname(), '.nc']); catch; ok = true; end; asserts.istrue(ok);
%=============================================================================



