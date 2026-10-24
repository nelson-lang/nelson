%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function print(varargin)
  nargoutchk(0, 0);

  fig = [];
  filename = '';
  device = '';
  resolution = '';

  for k = 1:nargin
    arg = varargin{k};
    if isStringScalar(arg) || (ischar(arg) && isrow(arg)) || isempty(arg)
      txt = localScalarText(arg);
      if startsWith(txt, '-d')
        device = lower(txt);
      elseif startsWith(txt, '-r')
        resolution = lower(txt);
      elseif startsWith(txt, '-')
        % other switches (e.g. '-loose', '-noui') are accepted and ignored.
      else
        filename = txt;
      end
    elseif isgraphics(arg, 'figure')
      fig = arg;
    elseif isnumeric(arg) && isscalar(arg) && isgraphics(arg, 'figure')
      fig = arg;
    else
      error('Nelson:print:invalidArgument', 'Unsupported argument for print.');
    end
  end

  if isempty(fig)
    fig = gcf();
  end
  if ~isgraphics(fig, 'figure')
    error('Nelson:print:expectedFigure', 'Expected figure graphics object.');
  end
  if isempty(filename)
    error('Nelson:print:filenameExpected', 'A filename is required to print to a file.');
  end

  % '-r<dpi>' is accepted for compatibility; the export resolution follows the
  % figure size and is not resampled.
  if ~isempty(resolution)
    dpi = resolution(3:end);
    if ~isempty(dpi) && isnan(str2double(dpi))
      error('Nelson:print:invalidResolution', 'Invalid resolution: %s', resolution);
    end
  end

  if isempty(device)
    saveas(fig, filename);
  else
    format = localDeviceToFormat(device);
    saveas(fig, filename, format);
  end
end
%=============================================================================
function txt = localScalarText(value)
  if isStringScalar(value)
    txt = char(value);
  elseif ischar(value)
    txt = value;
  else
    txt = '';
  end
end
%=============================================================================
function format = localDeviceToFormat(device)
  token = device(3:end);
  switch token
    case 'png'
      format = 'png';
    case {'jpeg', 'jpg'}
      format = 'jpg';
    case 'pdf'
      format = 'pdf';
    case 'svg'
      format = 'svg';
    case {'tiff', 'tiffn', 'tif'}
      format = 'tif';
    case {'bmp', 'bmpmono'}
      format = 'bmp';
    case 'gif'
      format = 'gif';
    case 'webp'
      format = 'webp';
    case 'pcx'
      format = 'pcx';
    case 'tga'
      format = 'tga';
    case {'pbm', 'pgm', 'ppm', 'pnm'}
      format = token;
    otherwise
      error('Nelson:print:unsupportedDevice', 'Unsupported device option: %s', device);
  end
end
%=============================================================================
