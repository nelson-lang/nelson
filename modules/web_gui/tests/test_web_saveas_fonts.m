%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--INTERACTIVE TEST-->
% <--WEB GUI REQUIRED--> run with: nelson-webview --file runtests_web.m
%=============================================================================
% The requested font must follow through the Qt-free exporters: a Times bold
% title maps to serif/bold in SVG and to the Times-Bold base-14 face in PDF,
% while default text stays sans-serif/Helvetica.
out = tempname();
if ~isfolder(out)
  mkdir(out);
end
cleanupOut = onCleanup(@() rmdir(out, 's')); %#ok<NASGU>
f = figure();
plot(1:10);
title('Titre Serif', 'FontName', 'Times New Roman', 'FontWeight', 'bold');
xlabel('x sans');
%=============================================================================
p = [out, '/fonts.svg'];
saveas(f, p);
txt = fileread(p);
assert_istrue(~isempty(strfind(txt, 'font-family="serif"')));
assert_istrue(~isempty(strfind(txt, 'font-weight="bold"')));
assert_istrue(~isempty(strfind(txt, 'font-family="sans-serif"')));
%=============================================================================
p = [out, '/fonts.pdf'];
saveas(f, p);
txt = fileread(p);
assert_istrue(~isempty(strfind(txt, '/Times-Bold')));
assert_istrue(~isempty(strfind(txt, '/Helvetica')));
% PDF numbers must never use scientific notation (invalid PDF; a rotated
% ylabel used to vanish because cos(90deg) serialized as 6e-17)
assert_istrue(isempty(strfind(txt, 'e-17')));
%=============================================================================
close(f);
%=============================================================================
