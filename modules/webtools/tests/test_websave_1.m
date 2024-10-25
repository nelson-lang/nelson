%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('websave'), -1);
assert_isequal(nargout('websave'), 1);
%=============================================================================
url = 'https://apod.nasa.gov/apod/image/2310/MoValleyEclipse.jpg';
filename = [tempdir(), 'MoValleyEclipse_2.jpg'];
options = weboptions('Timeout', 120);
try
  destination_filename = websave(filename, url, options);
catch ex
  R = strcmp(ex.message, _('Forbidden (403)')) || ...
      strcmp(ex.message, _('Timeout was reached')) || ... 
      strcmp(ex.message, _('Couldn''t resolve host name'));
  skip_testsuite(R, ex.message)
end
assert_istrue(isfile(destination_filename))
%=============================================================================
