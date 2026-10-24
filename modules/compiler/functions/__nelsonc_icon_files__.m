%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [files, sides, digest] = __nelsonc_icon_files__(filename, stage)
  sides = [16; 24; 32; 48; 64; 128; 256];
  [files, digest] = __nelsonc_image_files__(filename, stage, [sides, sides], false);
end
