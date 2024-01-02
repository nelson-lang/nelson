%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ADV-CLI MODE-->
%=============================================================================
TMPDIR = [tempdir, 'saveas_test/'];
if isdir(TMPDIR)
  rmdir(TMPDIR, 's');
end
mkdir(TMPDIR);
%=============================================================================
extensions = ["png", "jpg", "pdf", "svg"];
%=============================================================================
x = -2:0.25:2;
y = x;
[X,Y] = meshgrid(x);
F = X.*exp(-X.^2-Y.^2);
surf(X,Y,F);
%=============================================================================
for ex = extensions
  filename = [TMPDIR, 'export_image.', char(ex)];
  saveas(gcf(), filename);
  assert_istrue(isfile(filename));
  fileinfo = dir(filename);
  assert_istrue(fileinfo.bytes > 0);
end
%=============================================================================
filename = [TMPDIR, 'export_image.', 'blob'];
assert_checkerror('saveas(gcf(), filename)', [_('Unsupported format:'), 'blob']) 
%=============================================================================
filename = [TMPDIR, 'export_no_extension'];
for ex = extensions
  saveas(gcf(), filename, ex);
  assert_istrue(isfile([filename, '.', char(ex)]));
  fileinfo = dir([filename, '.', char(ex)]);
  assert_istrue(fileinfo.bytes > 0);
end
%=============================================================================
if isdir(TMPDIR)
  rmdir(TMPDIR, 's');
end
%=============================================================================
