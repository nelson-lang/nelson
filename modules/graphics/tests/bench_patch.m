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
tic()
f = figure();
f.Visible = 'off';
f.DrawLater = 'on';
f.Color = [0.1,0,0.05];
colormap('turbo');

t=0:(2*pi)/1023:2*pi;
r=2+sin(6*t)/2;
theta=t+sin(18*t)/18;
z0=r.*exp(theta*i);

hold on;
for s=1:-0.005:0.01
  z=exp(i*0.2*sin(10*s))*z0;
  xr=real(z);
  yr=imag(z);
  p = patch(s*xr, s*yr, s, 'EdgeColor', 'none');
end
hold off;
axis off;
f.DrawLater = 'off';
f.Visible = 'on';
toc()
%=============================================================================
