%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 8-->
% <--ADV-CLI MODE-->
% <--RELEASE ONLY-->
%=============================================================================
examples_directory = [modulepath('graphics', 'root'), '/', 'examples/'];
previousDemoDuration = getenv('NELSON_DEMO_DURATION');
setenv('NELSON_DEMO_DURATION', '1');
run([examples_directory, 'cube/demo_cube.m']);
setenv('NELSON_DEMO_DURATION', previousDemoDuration);
%=============================================================================
