%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 2-->
offState = nelson.lang.OnOffSwitchState('off');
onState = nelson.lang.OnOffSwitchState(true);
falseState = nelson.lang.OnOffSwitchState(false);
numericOffState = nelson.lang.OnOffSwitchState(0);
numericOnState = nelson.lang.OnOffSwitchState(1);
upperOffState = nelson.lang.OnOffSwitchState('OFF');
%=============================================================================
asserts.isequal(class(offState), 'nelson.lang.OnOffSwitchState');
asserts.istrue(isa(onState, 'nelson.lang.OnOffSwitchState'));
asserts.istrue(isa(onState, 'logical'));
%=============================================================================
asserts.isequal(char(offState), 'off');
asserts.isequal(char(onState), 'on');
asserts.isequal(char(falseState), 'off');
asserts.isequal(char(numericOffState), 'off');
asserts.isequal(char(numericOnState), 'on');
asserts.isequal(char(upperOffState), 'off');
asserts.isequal(logical(onState), true);
asserts.isequal(logical(falseState), false);
%=============================================================================
asserts.istrue(onState == nelson.lang.OnOffSwitchState.on);
asserts.istrue(offState == nelson.lang.OnOffSwitchState.off);
asserts.istrue(onState == true);
asserts.istrue(offState == false);
asserts.istrue(onState == 'on');
asserts.isfalse(onState == false);
%=============================================================================
displayText = evalc('nelson.lang.OnOffSwitchState(true)');
asserts.istrue(contains(displayText, 'nelson.lang.OnOffSwitchState'));
asserts.istrue(contains(displayText, 'enumeration'));
asserts.istrue(contains(displayText, 'on'));
asserts.isfalse(contains(displayText, 'with properties'));
asserts.isfalse(contains(displayText, 'Ordinal'));
%=============================================================================
% Behaves like the logical value it wraps.
asserts.istrue(islogical(onState));
asserts.isfalse(isnumeric(onState));
asserts.isequal(double(onState), 1);
asserts.isequal(double(offState), 0);
asserts.isequal(string(onState), "on");
asserts.isequal(string(offState), "off");
%=============================================================================
% strcmp/strcmpi compare the displayed text.
asserts.istrue(strcmp(onState, 'on'));
asserts.istrue(strcmp(offState, 'off'));
asserts.isfalse(strcmp(onState, 'off'));
asserts.istrue(strcmpi(onState, 'ON'));
asserts.isfalse(strcmp(onState, true));
%=============================================================================
% isequal stays backward-compatible: it matches the text like ==, and the
% underlying logical value otherwise.
asserts.istrue(isequal(onState, true));
asserts.istrue(isequal(offState, false));
asserts.istrue(isequal(onState, 'on'));
asserts.istrue(isequal(offState, 'off'));
asserts.isfalse(isequal(onState, 'off'));
asserts.istrue(isequal(onState, nelson.lang.OnOffSwitchState.on));
asserts.isfalse(isequal(onState, offState));
%=============================================================================
% switch dispatches through char().
asserts.isequal(local_switch(onState), 1);
asserts.isequal(local_switch(offState), 0);
%=============================================================================
% Concatenated with text an on/off state stands in for its 'on'/'off' char
% form, so it lowers to char (or string) instead of forcing the text operand
% into the enumeration. A concatenation of only states stays an enumeration.
asserts.isequal(['v=' onState], 'v=on');
asserts.isequal([onState '/' offState], 'on/off');
asserts.isequal(['a' onState 'b' offState 'c'], 'aonboffc');
asserts.isequal(class(['x' onState]), 'char');
asserts.isequal(['off'; offState], ['off'; 'off']);
asserts.isequal(["state=" onState], ["state=", "on"]);
asserts.isequal(class(["state=" onState]), 'string');
asserts.isequal(class([onState offState]), 'nelson.lang.OnOffSwitchState');
asserts.isequal(numel([onState offState]), 2);
%=============================================================================
function r = local_switch(state)
  switch state
    case 'on'
      r = 1;
    case 'off'
      r = 0;
    otherwise
      r = -1;
  end
end
%=============================================================================
