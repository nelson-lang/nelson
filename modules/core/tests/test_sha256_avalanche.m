%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ADV-CLI MODE-->
%=============================================================================
avalancheFigureVisible = 'off';
avalancheAnimationEnabled = true;
avalancheAnimationFrames = 12;
avalancheFramePause = 0;
avalancheAnimationContinuous = false;
run([modulepath('core', 'root'), '/examples/sha256_avalanche.m']);

assert_istrue(isgraphics(avalancheFigure));
assert_istrue(isgraphics(avalancheImage));
% The digest was parsed into 256 bits.
assert_isequal(numel(avalanchePrevBits), 256);
% SHA-256 produces 64 hex characters, and one input change avalanches widely.
digestOne = lower(sha256('Nelson block 1'));
digestTwo = lower(sha256('Nelson block 2'));
assert_isequal(numel(digestOne), 64);
assert_istrue(sum(digestOne ~= digestTwo) > 20);
delete(avalancheFigure);
drawnow();
%=============================================================================
