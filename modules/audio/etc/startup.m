%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
if strcmp(getenv('AUDIODEV'), 'null') == false && ~any(contains(argv(), '--noaudio'))
  addgateway(modulepath(nelsonroot(), 'audio', 'builtin'), 'audio');
  addpath(modulepath(nelsonroot(), 'audio', 'functions'), '-frozen');
end
%=============================================================================
