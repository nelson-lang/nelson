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
h = uicontrol();
R = properties(h);
REF = {'BackgroundColor';    
    'BeingDeleted';       
    'BusyAction';         
    'ButtonDownFcn';      
    'CData';              
    'Callback';           
    'Children';           
    'CreateFcn';          
    'DeleteFcn';          
    'Enable';             
    'FontAngle';          
    'FontName';           
    'FontSize';           
    'FontUnits';          
    'FontWeight';         
    'ForeGround';         
    'HandleVisibility';   
    'HorizontalAlignment';
    'Interruptible';      
    'KeyPressFcn';        
    'KeyReleaseFcn';      
    'ListboxTop';         
    'Max';                
    'Min';                
    'Parent';             
    'Position';           
    'SliderStep';         
    'String';             
    'Style';              
    'Tooltip';            
    'Type';               
    'UserData';           
    'Value';              
    'Visible'};
assert_isequal(R, REF);
%=============================================================================
