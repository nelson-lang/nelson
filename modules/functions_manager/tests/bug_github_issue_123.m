%=============================================================================
% Copyright (c) 2017 Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <-- Issue URL -->
% https://github.com/nelson-lang/nelson/issues/123
% <-- Short Description -->
% addpath stopped to work after repeatedly call to the same path.
%=============================================================================
addpath([nelsonroot(), '/modules/core/tests/']);
addpath([nelsonroot(), '/modules/core/tests/']);
addpath([nelsonroot(), '/modules/core/tests/']);

for k = 1:1000
  addpath(fileparts(nfilename('fullpathext')))
end