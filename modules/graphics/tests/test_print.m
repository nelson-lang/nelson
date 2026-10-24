%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ADV-CLI MODE-->
%=============================================================================
reset(groot());
f = figure();
plot(1:10, (1:10) .^ 2);
%=============================================================================
% -dpng, filename without extension: extension is appended.
base_png = tempname();
print(f, base_png, '-dpng');
file_png = [base_png, '.png'];
asserts.istrue(isfile(file_png));
rmfile(file_png);
%=============================================================================
% -dpdf with explicit extension.
file_pdf = [tempname(), '.pdf'];
print(f, file_pdf, '-dpdf');
asserts.istrue(isfile(file_pdf));
rmfile(file_pdf);
%=============================================================================
% -dsvg with an accepted (ignored) resolution switch.
file_svg = [tempname(), '.svg'];
print(f, file_svg, '-dsvg', '-r150');
asserts.istrue(isfile(file_svg));
rmfile(file_svg);
%=============================================================================
% No device: format inferred from the filename extension.
file_auto = [tempname(), '.png'];
print(f, file_auto);
asserts.istrue(isfile(file_auto));
rmfile(file_auto);
%=============================================================================
% Current figure used when no handle is given.
file_gcf = [tempname(), '.png'];
print(file_gcf, '-dpng');
asserts.istrue(isfile(file_gcf));
rmfile(file_gcf);
%=============================================================================
% Error: unsupported device.
assert_checkerror('print(gcf(), tempname(), ''-deps'')', getString(message('Nelson:print:unsupportedDevice', '-deps')), ...
  'Nelson:print:unsupportedDevice');
%=============================================================================
% Error: filename required.
assert_checkerror('print(gcf(), ''-dpng'')', getString(message('Nelson:print:filenameExpected')), ...
  'Nelson:print:filenameExpected');
%=============================================================================
% Error: invalid resolution.
assert_checkerror('print(gcf(), tempname(), ''-dpng'', ''-rabc'')', getString(message('Nelson:print:invalidResolution', '-rabc')), ...
  'Nelson:print:invalidResolution');
%=============================================================================
close all;
%=============================================================================
