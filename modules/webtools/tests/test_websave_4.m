%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
url = 'https://github.com/nelson-lang/nelson-website/blob/master/images/qml_demos.png';
filename = [tempdir(), 'qml_demos.png'];
options = weboptions('Timeout', Inf);
try
  destination_filename = websave(filename, url, options);
catch ex
  destination_filename = '';
  R = strcmp(ex.message, _('Forbidden (403)')) || ...
      strcmp(ex.message, _('Timeout was reached')) || ... 
      strcmp(ex.message, _('Couldn''t resolve host name'));
  skip_testsuite(R, ex.message)
end
assert_istrue(isfile(destination_filename));
info = dir(destination_filename);
assert_istrue(info.bytes > 0);
%=============================================================================
