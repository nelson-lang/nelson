%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ADV-CLI MODE-->
%=============================================================================
close all
%=============================================================================
% isvalid on a graphics handle reports whether the object still exists
f = figure('Visible', 'off');
assert_istrue(isvalid(f));
%=============================================================================
ax = axes('Parent', f);
s = surf(peaks(5), 'Parent', ax);
assert_istrue(isvalid(s));
assert_istrue(isvalid(ax));
%=============================================================================
% isvalid accepts an array of handles and returns one logical per element
h = [ax; s];
v = isvalid(h);
assert_isequal(v, [true; true]);
%=============================================================================
% a deleted object is no longer valid
delete(s);
assert_isfalse(isvalid(s));
%=============================================================================
% a closed figure handle is no longer valid
close(f);
assert_isfalse(isvalid(f));
%=============================================================================
