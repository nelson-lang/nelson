%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function out = datetimeDisplayText(obj)
  % Element texts for disp and display: an N-D array resolves the 'default'
  % format page by page (a page without time of day shows dates only),
  % while string, char and cellstr use one format for the whole array.
  serial = datenum(obj);
  out = strings(size(serial));
  if isempty(serial)
    return
  end
  pages = reshape(serial, size(serial, 1), size(serial, 2), []);
  pageSize = size(serial, 1) * size(serial, 2);
  for page = 1:size(pages, 3)
    pageSerial = pages(:, :, page);
    fmt = datetime.resolveDisplayFormat(obj.FormatValue, pageSerial);
    idx = (page - 1) * pageSize + (1:pageSize);
    out(idx) = datetimeRenderArray(pageSerial, fmt);
  end
end
%=============================================================================
