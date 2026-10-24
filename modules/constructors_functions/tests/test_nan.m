%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
R = nan([0, 3]);
REF = nan(0, 3);
asserts.isequal(R, REF);
%=============================================================================
R = nan([3, -3]);
REF = nan(3, 0);
asserts.isequal(R, REF);
asserts.isequal(size(R), [3, 0]);
%=============================================================================
% NaN(n) returns an n-by-n matrix (like zeros/ones), not an n-by-1 vector.
R = NaN(2);
asserts.isequal(size(R), [2, 2]);
asserts.istrue(all(isnan(R(:))));
%=============================================================================
R = NaN(3, 4);
asserts.isequal(size(R), [3, 4]);
%=============================================================================
% class argument: only 'single' and 'double' are supported.
R = NaN(2, 'single');
asserts.isequal(class(R), 'single');
asserts.isequal(size(R), [2, 2]);
asserts.istrue(all(isnan(R(:))));
%=============================================================================
R = NaN(2, 3, 'single');
asserts.isequal(class(R), 'single');
asserts.isequal(size(R), [2, 3]);
%=============================================================================
R = NaN('single');
asserts.isequal(class(R), 'single');
asserts.isequal(size(R), [1, 1]);
%=============================================================================
R = NaN(2, 'double');
asserts.isequal(class(R), 'double');
%=============================================================================
% 'like' prototype selects the class.
R = NaN(2, 'like', single(1));
asserts.isequal(class(R), 'single');
%=============================================================================
% Inf/NaN are floating-point: integer/logical classes are rejected.
asserts.checkerror('NaN(2, ''int8'')', _('A supported type expected at last argument.'));
asserts.checkerror('NaN(2, ''logical'')', _('A supported type expected at last argument.'));
%=============================================================================
