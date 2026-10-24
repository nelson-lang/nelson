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
% Bounded, headless run of the bi-level (PC-AC) ventilator monitor demo.
% VENT_MAXTICKS caps the drawnow animation loop so the demo returns.
%=============================================================================
addpath([modulepath('graphics', 'root'), '/examples/ventilator']);
global VENT_MAXTICKS
VENT_MAXTICKS = 25;
ventilator_gui();
global VENT
%=============================================================================
% single-compartment mechanics produce a realistic adult tidal volume
vti = sscanf(VENT.mVti.Text, '%d');
asserts.istrue(vti > 450 && vti < 750);
vte = sscanf(VENT.mVte.Text, '%d');
asserts.istrue(vte > 450 && vte < 750);
%=============================================================================
% plateau pressure sits between PEEP and Pinsp
pplat = str2double(VENT.mPlt.Text);
asserts.istrue(pplat > 14 && pplat < 20);
%=============================================================================
% the animation advanced and recorded trigger-marker events
asserts.istrue(VENT.t > 9.5);
asserts.istrue(numel(VENT.evI) >= 1);
%=============================================================================
% all the adjustable controls were created
asserts.istrue(ishandle(VENT.sPEEP));
asserts.istrue(ishandle(VENT.sTrig));
asserts.istrue(ishandle(VENT.sLeak));
asserts.istrue(ishandle(VENT.bHold));
asserts.istrue(ishandle(VENT.presetDrop));
%=============================================================================
delete(VENT.fig);
clear VENT_MAXTICKS
clear VENT
%=============================================================================
