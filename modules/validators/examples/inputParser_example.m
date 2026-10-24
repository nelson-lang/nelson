%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
p = inputParser();
addRequired(p, 'width', @(x) isnumeric(x) && isscalar(x) && x > 0);
addOptional(p, 'height', 1, @(x) isnumeric(x) && isscalar(x) && x > 0);
addParameter(p, 'units', 'm', @(x) ischar(x) || (isstring(x) && isscalar(x)));
parse(p, 4, 3, 'units', 'cm');
areaValue = p.Results.width * p.Results.height;
assert_isequal(areaValue, 12);
assert_isequal(p.Results.units, 'cm');
%=============================================================================
