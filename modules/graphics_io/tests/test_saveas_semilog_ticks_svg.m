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
addpath([modulepath('graphics_io'), '/tests/helpers']);
% <--SEQUENTIAL TEST REQUIRED-->
%=============================================================================
out = [tempdir(), 'saveas_semilog_ticks_svg'];
if ~isfolder(out)
  mkdir(out);
end
%=============================================================================
f = figure('Visible', 'off', 'Position', [100 100 560 420], 'Color', [1 1 1]);
x = logspace(-1, 2);
semilogx(x, x);
grid on;
svgFile = [out, '/semilogx.svg'];
saveas(f, svgFile);
close(f);
%=============================================================================
svgInfo = assert_valid_svg(svgFile, [560 420], {'polygon', 'line', 'text'});
asserts.istrue(numel(regexp(svgInfo.text, '<line ', 'match')) >= 20);
%=============================================================================
f = figure('Visible', 'off', 'Position', [100 100 560 420], 'Color', [1 1 1]);
x = logspace(-1, 2);
semilogy(x, x);
grid on;
svgFile = [out, '/semilogy.svg'];
saveas(f, svgFile);
close(f);
%=============================================================================
svgInfo = assert_valid_svg(svgFile, [560 420], {'polygon', 'line', 'text'});
asserts.istrue(numel(regexp(svgInfo.text, '<line ', 'match')) >= 20);
%=============================================================================
