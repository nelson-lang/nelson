%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function T = ip_registration_affine_matrix(imgSize, angleDegrees, scaleX, scaleY, shear)
  rows = imgSize(1);
  cols = imgSize(2);
  cx = (cols + 1) / 2;
  cy = (rows + 1) / 2;
  theta = angleDegrees * pi / 180;
  c = cos(theta);
  s = sin(theta);
  toOrigin = [1 0 0; 0 1 0; -cx -cy 1];
  fromOrigin = [1 0 0; 0 1 0; cx cy 1];
  affinePart = [scaleX 0 0; shear scaleY 0; 0 0 1] ...
    * [c s 0; -s c 0; 0 0 1];
  T = toOrigin * affinePart * fromOrigin;
end
%=============================================================================
