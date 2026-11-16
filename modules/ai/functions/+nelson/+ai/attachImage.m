%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Attach an image file to options.Images (base64) so a vision model can see it.
function options = attachImage(options, imagePath)
  narginchk(2, 2);
  if isempty(imagePath) || ~isfile(imagePath)
    return
  end
  fid = fopen(imagePath, 'rb');
  if fid < 0
    return
  end
  bytes = fread(fid, inf, 'uint8=>uint8');
  fclose(fid);
  encoded = nelson.io.base64encode(bytes);
  if isfield(options, 'Images') && iscell(options.Images)
    options.Images{end + 1} = encoded;
  else
    options.Images = {encoded};
  end
end
%=============================================================================
