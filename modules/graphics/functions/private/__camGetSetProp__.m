%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Shared get/set/mode logic for the cam* property accessors
% (campos, camtarget, camva, camup). propName is the axes property,
% modeName its companion <prop>Mode property.
function out = __camGetSetProp__(ax, propName, modeName, args, nout)
  out = [];
  if (isempty(args))
    out = get(ax, propName);
    return
  end
  value = args{1};
  if (ischar(value) || isStringScalar(value))
    v = lower(char(value));
    switch (v)
      case {'auto', 'manual'}
        set(ax, modeName, v);
      case 'mode'
        out = get(ax, modeName);
      otherwise
        error('Nelson:graphics:invalidCamOption', ...
          'Value must be ''auto'', ''manual'' or ''mode''.');
    end
    return
  end
  set(ax, propName, value);
  if (nout > 0)
    out = get(ax, propName);
  end
end
%=============================================================================
