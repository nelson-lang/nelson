%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function d = im2double(img, varargin)
  % scaled on the device, other forms and errors come from the host
  narginchk(1, 2);
  nargoutchk(0, 1);
  scale = struct('uint8', 255, 'uint16', 65535);
  cls = underlyingType(img);
  if nargin == 1 && any(strcmp(cls, {'single', 'double', 'logical'}))
    d = single(img);
  elseif nargin == 1 && isfield(scale, cls)
    d = single(img) / scale.(cls);
  elseif nargin == 1 && strcmp(cls, 'int16')
    d = (single(img) + 32768) / 65535;
  else
    d = __gpuArray_host__('im2double', img, varargin{:});
  end
end
%=============================================================================
