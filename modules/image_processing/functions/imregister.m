%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function registered = imregister(varargin)
  narginchk(5, 13);
  nargoutchk(0, 1);
  options = ip_parse_registration_output_options(varargin(6:nargin));
  tform = imregtform(varargin{:});
  fixed = varargin{2};
  outputSize = [size(fixed, 1), size(fixed, 2)];
  registered = imwarp(varargin{1}, tform, 'Interpolation', options.Interpolation, ...
    'OutputView', outputSize);
end
%=============================================================================
