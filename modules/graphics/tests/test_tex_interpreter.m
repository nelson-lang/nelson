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
tex = {'\alpha';
'\upsilon';
'\sim';
'\angle';
'\phi';
'\leq';
'\ast';
'\chi';
'\infty';
'\beta';
'\psi';
'\clubsuit';
'\gamma';
'\omega';
'\diamondsuit';
'\delta';
'\Gamma';
'\heartsuit';
'\epsilon';
'\Delta';
'\spadesuit';
'\zeta';
'\Theta';
'\leftrightarrow';
'\eta';
'\Lambda';
'\leftarrow';
'\theta';
'\Xi';
'\Leftarrow';
'\vartheta';
'\Pi';
'\uparrow';
'\iota';
'\Sigma';
'\rightarrow';
'\kappa';
'\Upsilon';
'\Rightarrow';
'\lambda';
'\Phi';
'\downarrow';
'\mu';
'\Psi';
'\circ';
'\nu';
'\Omega';
'\pm';
'\xi';
'\forall';
'\geq';
'\pi';
'\exists';
'\propto';
'\rho';
'\ni';
'\partial';
'\sigma';
'\cong';
'\bullet';
'\varsigma';
'\approx';
'\div';
'\tau';
'\Re';
'\neq';
'\equiv';
'\oplus';
'\aleph';
'\Im';
'\cup';
'\wp';
'\otimes';
'\subseteq';
'\oslash';
'\cap';
'\in';
'\supseteq';
'\supset';
'\lceil';
'\subset';
'\int';
'\cdot';
'\o';
'\rfloor';
'\neg';
'\nabla';
'\lfloor';
'\times';
'\ldots';
'\perp';
'\surd';
'\prime';
'\wedge';
'\varpi';
'\0';
'\rceil';
'\rangle';
'\mid';
'\vee';
'\langle';
'\copyright'};

f = figure();
ax = gca();
xlim([0, 300]);
ylim([0, 130]);
j = 1;
i = 1;
line = [];

for t = tex'
  line = [line, t{1}];
  if (j == 10)
    text(30, i*10, line);
    line = [];
    i = i + 1;
    j = 1;
  else
    j = j + 1;
  end
end
