%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = camproj(varargin)
  % camproj, camproj(ax)                     -> query projection
  % camproj('orthographic'|'perspective')    -> set projection
  % camproj(ax, ...)
  narginchk(0, 2);
  nargoutchk(0, 1);
  [ax, args] = __camAxesArgs__(varargin);
  if (isempty(args))
    varargout{1} = get(ax, 'Projection');
    return
  end
  value = args{1};
  if (~(ischar(value) || isStringScalar(value)))
    error('Nelson:graphics:invalidProjection', ...
      'Projection must be ''orthographic'' or ''perspective''.');
  end
  v = lower(char(value));
  % Accept the shortest unambiguous leading substring (e.g. 'p' -> 'perspective',
  % 'o' -> 'orthographic'), matching how the Projection property itself resolves.
  choices = {'orthographic', 'perspective'};
  match = ~isempty(v) & startsWith(choices, v);
  if (sum(match) ~= 1)
    error('Nelson:graphics:invalidProjection', ...
      'Projection must be ''orthographic'' or ''perspective''.');
  end
  v = choices{match};
  set(ax, 'Projection', v);
  if (nargout > 0)
    varargout{1} = get(ax, 'Projection');
  else
    varargout = {};
  end
end
%=============================================================================
