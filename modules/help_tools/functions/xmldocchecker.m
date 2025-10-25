%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = xmldocchecker(varargin)
    narginchk(0, 1);
    nargoutchk(0, 3);
    varargout = {};
    if nargin == 0
        modules_path = [nelsonroot(), '/modules/'];
        module_list = dir(modules_path);
        disp(_('Checking XML documentation...'));
        for i = 1:length(module_list)
            if module_list(i).isdir && ~ismember(module_list(i).name, {'.', '..'})
                module_name = module_list(i).name;
                for lang = getavailablelanguages()'
                    help_path = [modules_path, module_name, '/help/', lang{1}, '/xml/'];
                    if isdir(help_path)
                        xmlfiles = dir([help_path, '*.xml']);
                        for j = 1:length(xmlfiles)
                            xmlfile = [help_path, xmlfiles(j).name];
                            switch nargout()
                            case 0
                                xmldocchecker(xmlfile);
                                msg = sprintf(_('XML documentation file "%s" is valid.'), xmlfile);
                                disp(msg);
                            case 1
                                state = xmldocchecker(xmlfile);
                                if ~state
                                    varargout{1} = state;
                                    return
                                end
                            case 2
                                [state, errors_detected] = xmldocchecker(xmlfile);
                                if ~state
                                    varargout{1} = state;
                                    varargout{2} = errors_detected;
                                    return
                                end
                            case 3
                                [state, errors_detected, warnings_detected] = xmldocchecker(xmlfile);
                                if ~state
                                    varargout{1} = state;
                                    varargout{2} = errors_detected;
                                    varargout{3} = warnings_detected;
                                    return
                                end
                            otherwise
                                varargout = {};
                            end
                        end
                    end
                end
            end
        end
        switch nargout()
            case 0
                varargout = {};
            case 1
                varargout{1} = true;
            case 2
                varargout{1} = true;
                varargout{2} = {};
            case 3
                varargout{1} = true;
                varargout{2} = {};
                varargout{3} = {};
            otherwise
                varargout = {};
        end
        return;
    end

    xmldocfilename = varargin{1};
    % Check if the file exists
    if ~isfile(xmldocfilename)
        error('File not found: %s', xmldocfilename);
    end
    xsd_xmldoc = [modulepath('help_tools'), '/resources/nelson_help.xsd'];
    % Validate the XML document
    switch nargout()
        case 0
            xmlchecker(xmldocfilename, xsd_xmldoc);
        case 1
            state = xmlchecker(xmldocfilename, xsd_xmldoc);
            varargout{1} = state;
        case 2
              [state, errors_detected, warnings_detected] = xmlchecker(xmldocfilename, xsd_xmldoc);
 
            varargout = {state, errors_detected};
        case 3
            [state, errors_detected, warnings_detected] = xmlchecker(xmldocfilename, xsd_xmldoc);
           
            varargout = {state, errors_detected, warnings_detected};
        otherwise
            varargout = {};
    end
end
%=============================================================================
