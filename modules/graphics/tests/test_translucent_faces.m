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
% A translucent 3D surface (FaceAlpha < 1) must composite its overlapping faces
% back-to-front and stay saturated, matching the reference and the software
% backends. Two defects washed such a surface out and both are guarded here:
%   - the faces were painted in model order, so the depth buffer culled the back
%     faces instead of letting them blend through;
%   - the OpenGL frame grab drove the destination alpha below one, so flattening
%     the RGBA frame over the white page lightened the whole surface.
% A washed-out render collapses toward white: its coloured pixels lose most of
% their saturation. A correct render keeps the colormap vivid, so the mean
% saturation of the coloured pixels stays high on every backend.
%=============================================================================
close all
%=============================================================================
rng(42, 'twister');
P = randn(30, 3);
T = convhulln(P);
%=============================================================================
% Qt software backend (hidden figure getframe): the translucent hull must stay
% saturated, not wash out toward white.
f = figure('Visible', 'off', 'Position', [100 100 560 420], 'Color', [1 1 1]);
trisurf(T, P(:, 1), P(:, 2), P(:, 3), 'FaceAlpha', 0.4);
drawnow();
meanSat = colored_mean_saturation(getframe(f).cdata);
close(f);
asserts.istrue(meanSat > 0.28);
%=============================================================================
% OpenGL backend (visible figure getframe): same surface, same requirement. This
% is the path that washed out before the back-to-front sort and the opaque-alpha
% frame grab were fixed.
f = figure('Visible', 'on', 'Position', [100 100 560 420], 'Color', [1 1 1]);
trisurf(T, P(:, 1), P(:, 2), P(:, 3), 'FaceAlpha', 0.4);
drawnow();
meanSat = colored_mean_saturation(getframe(f).cdata);
close(f);
asserts.istrue(meanSat > 0.28);
%=============================================================================
% A translucent surf must also stay saturated on the OpenGL backend.
f = figure('Visible', 'on', 'Position', [100 100 560 420], 'Color', [1 1 1]);
s = surf(peaks);
set(s, 'FaceAlpha', 0.4);
drawnow();
meanSat = colored_mean_saturation(getframe(f).cdata);
close(f);
asserts.istrue(meanSat > 0.20);
%=============================================================================
function meanSat = colored_mean_saturation(cdata)
  % Mean HSV-style saturation ((max - min) / max) over the coloured pixels of a
  % frame -- the grey/white axes and page are excluded so only the surface counts.
  c = double(cdata) / 255;
  r = c(:, :, 1);
  g = c(:, :, 2);
  b = c(:, :, 3);
  mx = max(max(r, g), b);
  mn = min(min(r, g), b);
  sat = zeros(size(mx));
  nonzero = mx > 0;
  sat(nonzero) = (mx(nonzero) - mn(nonzero)) ./ mx(nonzero);
  colored = (mx - mn) > (25 / 255);
  if ~any(colored(:))
    meanSat = 0;
    return
  end
  meanSat = mean(sat(colored));
end
%=============================================================================
