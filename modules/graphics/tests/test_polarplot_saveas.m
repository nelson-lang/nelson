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
% Exporting a polar plot must complete and produce a file on every backend;
% it must not block the renderer.
%=============================================================================
reset(groot());
th = linspace(0, 2 * pi, 200);
polarplot(th, abs(sin(3 * th)));
drawnow();
%=============================================================================
% saveas: raster export (PNG).
file_png = [tempname(), '.png'];
saveas(gcf(), file_png);
asserts.istrue(isfile(file_png));
asserts.istrue(localFileSize(file_png) > 0);
rmfile(file_png);
%=============================================================================
% saveas: vector export (SVG).
file_svg = [tempname(), '.svg'];
saveas(gcf(), file_svg);
asserts.istrue(isfile(file_svg));
rmfile(file_svg);
%=============================================================================
% saveas: document export (PDF).
file_pdf = [tempname(), '.pdf'];
saveas(gcf(), file_pdf);
asserts.istrue(isfile(file_pdf));
rmfile(file_pdf);
%=============================================================================
% print: same figure through the -d device path.
file_print = [tempname(), '.png'];
print(gcf(), file_print, '-dpng');
asserts.istrue(isfile(file_print));
rmfile(file_print);
%=============================================================================
close all;
%=============================================================================
function sz = localFileSize(filename)
  info = dir(filename);
  sz = info.bytes;
end
%=============================================================================
