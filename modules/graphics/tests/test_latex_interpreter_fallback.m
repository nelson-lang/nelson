%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 2-->
% <--ADV-CLI MODE-->
%=============================================================================
% The 'latex' interpreter has no full math layout engine yet: it falls back to
% the 'tex' pipeline (symbol substitution + super/subscripts) after stripping a
% surrounding pair of $...$ math delimiters. See
% docs/design/latex_math_interpreter.md.
%=============================================================================
f = figure('Visible', 'off');
ax = axes('Parent', f);
%=============================================================================
% 'latex' is an accepted Interpreter value and leaves String untouched.
t = text(0.5, 0.5, '$\alpha_0$', 'Interpreter', 'latex');
assert_isequal(get(t, 'Interpreter'), 'latex');
assert_isequal(get(t, 'String'), '$\alpha_0$');
%=============================================================================
% Surrounding $...$ are stripped: the laid-out text is narrower than the same
% string drawn verbatim by the 'none' interpreter.
tl = text(0.5, 0.5, '$x$', 'Interpreter', 'latex');
tn = text(0.5, 0.5, '$x$', 'Interpreter', 'none');
drawnow();
extentLatex = get(tl, 'Extent');
extentNone = get(tn, 'Extent');
assert_istrue(extentLatex(3) < extentNone(3));
%=============================================================================
% Symbols are substituted: '\alpha' renders as one glyph, much narrower than the
% six literal characters shown by the 'none' interpreter.
ta = text(0.5, 0.5, '\alpha', 'Interpreter', 'latex');
tt = text(0.5, 0.5, '\alpha', 'Interpreter', 'none');
drawnow();
extentAlphaLatex = get(ta, 'Extent');
extentAlphaNone = get(tt, 'Extent');
assert_istrue(extentAlphaLatex(3) < extentAlphaNone(3));
%=============================================================================
% Rendering the fallback through getframe does not error.
title('$\int_0^\infty e^{-x^2}\,dx = \frac{\sqrt{\pi}}{2}$', 'Interpreter', 'latex');
frame = getframe(f);
assert_istrue(size(frame.cdata, 3) == 3);
assert_istrue(all(size(frame.cdata) > 0));
%=============================================================================
