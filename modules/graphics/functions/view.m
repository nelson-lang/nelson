%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function view(varargin)
  % view(2), view(3)
  % view([0 90]), view([0 90 0])
  % view(az, el)
  
  narginchk(1, 3);
  if nargin == 1
    ax = gca();  
    value = varargin{1};
    [az, el] = viewValue(ax, value);
  else
    if nargin == 2
      if (isgraphics(varargin{1}))
        ax = varargin{1};
        value = varargin{2};
        [az, el] = viewValue(ax, value);
      else
        ax = gca();  
        az = varargin{1};
        el = varargin{2};
      end
    else
      ax = varargin{1};  
      az = varargin{2};
      el = varargin{3};
    end
  end
  applyView(ax, az, el)
end
%=============================================================================
function applyView(ax, az, el)
  az = az * (pi / 180);
  if (el == 0)
    el = 0.001;
  end
  el = el * (pi / 180);
  xlim = ax.XLim;
  xmean = mean(xlim);
  ylim = ax.YLim;
  ymean = mean(ylim);
  zlim = ax.ZLim;
  zmean = mean(zlim);
  xmax = max(abs(xlim - xmean));
  ymax = max(abs(ylim - ymean));
  zmax = max(abs(zlim - zmean));
  r = sqrt(xmax^2 + ymax^2 + zmax^2);
  z = sin(el) * r + zmean;
  y = -cos(el) * cos(az) * r + ymean;
  x = cos(el)*sin(az)*r + xmean;
  ax.CameraPosition = [x, y, z];
  
  if (abs(el-pi/2) < .001)
    ax.CameraUpVector = [0, 1, 0];
  else
    ax.CameraUpVector = [0, 0, 1];
  end
  
end
%=============================================================================
function [az, el] = viewValue(ax, value)
  if isscalar(value)
    if (value == 2)
      az = 0;
      el = 90;
      if ~strcmpi(ax.CameraUpVectorMode, 'auto')
        if strcmpi(ax.YDir, 'normal')
          ax.CameraUpVector = [0 1 0];
        else
          ax.CameraUpVector = [0 -1 0];
        end
        cameraUpVectorUpdated = true;
      end
    elseif (value == 3)
      az = -37.5;
      el = 30;
      if ~strcmpi(ax.CameraUpVectorMode, 'auto')
        ax.CameraUpVector = [0 0 1];
        cameraUpVectorUpdated = true;
      end
    else
      error(_('Invalid input arguments.'));
    end
  elseif (length(value) == 2)
    az = value(1);
    el = value(2);
  elseif (length(value) == 3)
    x = value(1);
    y = value(2);
    z = value(3);
    unit = value / norm(value);
    az = atan2(x, y) * 180 / pi;
    el = atan2(z, sqrt(x^2 + y^2)) * 180 / pi;
  else
    error(_('Invalid input arguments.'));
  end
end
%=============================================================================
