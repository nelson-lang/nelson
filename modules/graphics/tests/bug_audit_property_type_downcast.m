%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 4-->
% <--ADV-CLI MODE-->
%=============================================================================
% Regression: several fixed-size vector properties were registered as / downcast
% to the wrong concrete property type (UBSan type confusion): axes View was a
% base GOVectorProperty cast to GOTwoVectorProperty, image XData/YData likewise,
% and figure Color is a GOColorProperty set through setThreeVectorDefault (a
% sibling of GOThreeVectorProperty). Verify the values round-trip correctly.
%=============================================================================
f = figure();
ax = axes();
view(ax, 30, 45);
v = ax.View;
asserts.isapprox(v(:)', [30 45], 1e-9);
%=============================================================================
im = image(magic(4));
asserts.isequal(im.XData, [1 4]);
asserts.isequal(im.YData, [1 4]);
im.XData = [0 8];
asserts.isequal(im.XData, [0 8]);
%=============================================================================
% figure Color is a GOColorProperty set via setThreeVectorDefault; just make
% sure it is a valid 3-element color and did not corrupt anything.
c = f.Color;
asserts.istrue(numel(c) == 3);
asserts.istrue(all(c >= 0 & c <= 1));
%=============================================================================
close(f);
%=============================================================================
