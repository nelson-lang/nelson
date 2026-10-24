%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function tf = usejava(feature)
  narginchk(1, 1);
  nargoutchk(0, 1);
  if ~(ischar(feature) || (isstring(feature) && isscalar(feature)))
    error('Nelson:types:wrongArgumentTypeExpected', _('Wrong type for argument #%d: %s expected.'), 1, 'string');
  end
  % 'desktop' is intentionally not supported: it is a deprecated feature.
  supported = {'awt', 'jvm', 'swing'};
  if ~any(strcmp(char(feature), supported))
    error('Nelson:usejava:invalidFeature', _('Feature must be one of the following: ''awt'', ''jvm'', ''swing''.'));
  end
  % Nelson embeds no JVM, so no Java capability is ever available.
  tf = logical(false);
end
%=============================================================================
