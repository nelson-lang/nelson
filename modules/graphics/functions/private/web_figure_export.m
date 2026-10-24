%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function result = web_figure_export(f, request)
  format = lower(request.format);
  if ~any(strcmp(format, {'png', 'svg', 'pdf'}))
    error('Nelson:graphics:invalidExport', 'PNG, SVG or PDF format expected.');
  end
  scale = request.scale;
  if ~isscalar(scale) || ~isfinite(scale) || scale < 1 || scale > 4
    error('Nelson:graphics:invalidExportScale', 'Export scale must be between one and four.');
  end
  original = jsondecode(__web_display_list__(f));
  scale = min(scale, 4096 / max(original.width, original.height));
  width = ceil(original.width * scale);
  height = ceil(original.height * scale);
  path = [tempname(), '.', format];
  fd = -1;
  try
    __web_export_graphics__(f, path, scale);
    fd = fopen(path, 'rb');
    if fd < 0
      error('Nelson:graphics:exportFailed', 'Cannot read exported figure.');
    end
    data = fread(fd, Inf, '*uint8');
    fclose(fd); fd = -1;
    delete(path);
  catch exception
    if fd >= 0
      fclose(fd);
    end
    if isfile(path)
      delete(path);
    end
    rethrow(exception);
  end
  result = struct('format', format, 'bytes', data(:)', 'width', width, 'height', height);
end
