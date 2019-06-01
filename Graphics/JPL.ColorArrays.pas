unit JPL.ColorArrays;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}

{
  https://en.wikipedia.org/wiki/Web_colors
}

interface

uses

  //JP.Colors,
  //Classes,
  //SysUtils,
  Graphics;

type

  TColorListType = (
    cltBasic,
    cltStandard16, cltStandard48, cltWin10Theme, cltSystem,
    cltWebGrayBlack, cltWebWhite,
    cltWebPink, cltWebRed, cltWebOrange, cltWebYellow, cltWebBrown,
    cltWebGreen, cltWebCyan, cltWebBlue, cltWebPurpleVioletMagenta,
    cltWeb216Safe
  );

  TColorListSet = set of TColorListType;

  TColorArrayItem = record
    Color: TColor;
    Name: string;
  end;


const

  {$region ' ------ Colors_Basic ------ '}
  Colors_Basic: array[0..38] of TColorArrayItem = (
    (Color: $00000000; Name: 'Black'),
    (Color: $00333333; Name: 'Gray 80%'),
    (Color: $00808080; Name: 'Gray 50%'),
    (Color: $00A5A5A5; Name: 'Gray 35%'),
    (Color: $00C0C0C0; Name: 'Gray 25%'),
    (Color: $00D9D9D9; Name: 'Gray 15%'),
    (Color: $00F2F2F2; Name: 'Gray 5%'),
    (Color: $00F0FBFF; Name: 'Cream'),
    (Color: $00FFFFFF; Name: 'White'),

    (Color: $00003333; Name: 'Dark Brown'),
    (Color: $00000080; Name: 'Maroon'),
    (Color: $00003399; Name: 'Brown'),
    (Color: $000000FF; Name: 'Red'),
    (Color: $000066FF; Name: 'Orange'),
    (Color: $000099FF; Name: 'Light Orange'),
    (Color: $0000CCFF; Name: 'Gold'),
    (Color: $0099CCFF; Name: 'Beige'),
    (Color: $0000FFFF; Name: 'Yellow'),
    (Color: $0099FFFF; Name: 'Light Yellow'),

    (Color: $00008080; Name: 'Olive'),
    (Color: $00003300; Name: 'Dark Green'),
    (Color: $00008000; Name: 'Green'),
    (Color: $00808000; Name: 'Teal'),
    (Color: $00669933; Name: 'Sea'),
    (Color: $0000CC99; Name: 'Light Green'),
    (Color: $0000FF00; Name: 'Lime'),
    (Color: $00C0DCC0; Name: 'Money Green'),
    (Color: $00CCFFCC; Name: 'Pale Green'),

    (Color: $00800000; Name: 'Navy'),
    (Color: $00993333; Name: 'Indigo'),
    (Color: $00FF0000; Name: 'Blue'),
    (Color: $00FF6633; Name: 'Light Blue'),
    (Color: $00FFCC00; Name: 'Azure'),
    (Color: $00FFCC99; Name: 'Pale Blue'),
    (Color: $00FFFF00; Name: 'Aqua'),
    (Color: $00CCCC33; Name: 'Aquamarine'),

    (Color: $00800080; Name: 'Purple'),
    (Color: $00FF00FF; Name: 'Fuchsia'),
    (Color: $00663399; Name: 'Plum')
  );
  {$endregion Colors_Basic}


  {$region ' ------ Colors_System ------ '}
  Colors_System: array[0..29] of TColorArrayItem = (
    (Color: clActiveBorder; Name: 'Active Border'),
    (Color: clActiveCaption; Name: 'Active Caption'),
    (Color: clAppWorkSpace; Name: 'Application Workspace'),
    (Color: clBackground; Name: 'Background'),
    (Color: clBtnFace; Name: 'Button Face'),
    (Color: clBtnHighlight; Name: 'Button Highlight'),
    (Color: clBtnShadow; Name: 'Button Shadow'),
    (Color: clBtnText; Name: 'Button Text'),
    (Color: clCaptionText; Name: 'Caption Text'),
    (Color: clGradientActiveCaption; Name: 'Gradient Active Caption'),
    (Color: clGradientInactiveCaption; Name: 'Gradient Inactive Caption'),
    (Color: clGrayText; Name: 'Gray Text'),
    (Color: clHighlight; Name: 'Highlight Background'),
    (Color: clHighlightText; Name: 'Highlight Text'),
    (Color: clHotLight; Name: 'Hot Light'),
    (Color: clInactiveBorder; Name: 'Inactive Border'),
    (Color: clInactiveCaption; Name: 'Inactive Caption'),
    (Color: clInactiveCaptionText; Name: 'Inactive Caption Text'),
    (Color: clInfoBk; Name: 'Info Background'),
    (Color: clInfoText; Name: 'Info Text'),
    (Color: clMenu; Name: 'Menu Background'),
    (Color: clMenuBar; Name: 'Menu Bar'),
    (Color: clMenuHighlight; Name: 'Menu Highlight'),
    (Color: clMenuText; Name: 'Menu Text'),
    (Color: clScrollBar; Name: 'Scroll Bar'),
    (Color: cl3DDkShadow; Name: '3D Dark Shadow'),
    (Color: cl3DLight; Name: '3D Light'),
    (Color: clWindow; Name: 'Window Background'),
    (Color: clWindowFrame; Name: 'Window Frame'),
    (Color: clWindowText; Name: 'Window Text')
  );
  {$endregion Colors_System}



  {$region ' ------ Colors_Win10Theme ------ '}
  Colors_Win10Theme: array[0..47] of TColorArrayItem = (
    (Color: $0000B9FF; Name: ''),
    (Color: $00008CFF; Name: ''),
    (Color: $000C63F7; Name: ''),
    (Color: $001050CA; Name: ''),
    (Color: $00013BDA; Name: ''),
    (Color: $005069EF; Name: ''),
    (Color: $003834D1; Name: ''),
    (Color: $004343FF; Name: ''),
    (Color: $005648E7; Name: ''),
    (Color: $002311E8; Name: ''),
    (Color: $005E00EA; Name: ''),
    (Color: $005200C3; Name: ''),
    (Color: $008C00E3; Name: ''),
    (Color: $007700BF; Name: ''),
    (Color: $00B339C2; Name: ''),
    (Color: $0089009A; Name: ''),
    (Color: $00D77800; Name: ''),
    (Color: $00B16300; Name: ''),
    (Color: $00D88C8E; Name: ''),
    (Color: $00D6696B; Name: ''),
    (Color: $00B86487; Name: ''),
    (Color: $00A94D74; Name: ''),
    (Color: $00C246B1; Name: ''),
    (Color: $00981788; Name: ''),
    (Color: $00BC9900; Name: ''),
    (Color: $009A7D2D; Name: ''),
    (Color: $00C3B700; Name: ''),
    (Color: $00878303; Name: ''),
    (Color: $0094B200; Name: ''),
    (Color: $00748501; Name: ''),
    (Color: $006ACC00; Name: ''),
    (Color: $003E8910; Name: ''),
    (Color: $0074757A; Name: ''),
    (Color: $00585A5D; Name: ''),
    (Color: $008A7668; Name: ''),
    (Color: $006B5C51; Name: ''),
    (Color: $00737C56; Name: ''),
    (Color: $00606848; Name: ''),
    (Color: $00058249; Name: ''),
    (Color: $00107C10; Name: ''),
    (Color: $00767676; Name: ''),
    (Color: $00484A4C; Name: ''),
    (Color: $007E7969; Name: ''),
    (Color: $0059544A; Name: ''),
    (Color: $00647C64; Name: ''),
    (Color: $00545E52; Name: ''),
    (Color: $00457584; Name: ''),
    (Color: $005F737E; Name: '')
  );
  {$endregion Colors_Win10Theme}
              
       
  {$region ' ------ Colors_Standard16 ------ '}
  Colors_Standard16: array[0..15] of TColorArrayItem = (
    (Color: $00000000; Name: 'Black'),
    (Color: $00000080; Name: 'Maroon'),
    (Color: $00008000; Name: 'Green'),
    (Color: $00008080; Name: 'Olive'),
    (Color: $00800000; Name: 'Navy'),
    (Color: $00800080; Name: 'Purple'),
    (Color: $00808000; Name: 'Teal'),
    (Color: $00C0C0C0; Name: 'Silver'),
    (Color: $00808080; Name: 'Gray'),
    (Color: $000000FF; Name: 'Red'),
    (Color: $0000FF00; Name: 'Lime'),
    (Color: $0000FFFF; Name: 'Yellow'),
    (Color: $00FF0000; Name: 'Blue'),
    (Color: $00FF00FF; Name: 'Fuchsia'),
    (Color: $00FFFF00; Name: 'Aqua'),
    (Color: $00FFFFFF; Name: 'White')
  );
  {$endregion}  
            
  
  {$region ' ------ Colors_Standard48 ------ '}
  // Windows color dialog
  Colors_Standard48: array[0..47] of TColorArrayItem = (
    (Color: $008080FF; Name: ''),
    (Color: $0080FFFF; Name: ''),
    (Color: $0080FF80; Name: ''),
    (Color: $0080FF00; Name: ''),
    (Color: $00FFFF80; Name: ''),
    (Color: $00FF8000; Name: ''),
    (Color: $00C080FF; Name: ''),
    (Color: $00FF80FF; Name: ''),
    (Color: $000000FF; Name: 'Red'),
    (Color: $0000FFFF; Name: 'Yellow'),
    (Color: $0000FF80; Name: ''),
    (Color: $0040FF00; Name: ''),
    (Color: $00FFFF00; Name: 'Aqua'),
    (Color: $00C08000; Name: ''),
    (Color: $00C08080; Name: ''),
    (Color: $00FF00FF; Name: 'Pink'),
    (Color: $00404080; Name: ''),
    (Color: $004080FF; Name: ''),
    (Color: $0000FF00; Name: 'Lime'),
    (Color: $00808000; Name: 'Teal'),
    (Color: $00804000; Name: ''),
    (Color: $00FF8080; Name: ''),
    (Color: $00400080; Name: ''),
    (Color: $008000FF; Name: ''),
    (Color: $00000080; Name: 'Maroon'),
    (Color: $000080FF; Name: ''),
    (Color: $00008000; Name: 'Green'),
    (Color: $00408000; Name: ''),
    (Color: $00FF0000; Name: 'Blue'),
    (Color: $00A00000; Name: ''),
    (Color: $00800080; Name: 'Purple'),
    (Color: $00FF0080; Name: ''),
    (Color: $00000040; Name: ''),
    (Color: $00004080; Name: ''),
    (Color: $00004000; Name: ''),
    (Color: $00404000; Name: ''),
    (Color: $00800000; Name: 'Navy'),
    (Color: $00400000; Name: ''),
    (Color: $00400040; Name: ''),
    (Color: $00800040; Name: ''),
    (Color: $00000000; Name: 'Black'),
    (Color: $00008080; Name: 'Olive'),
    (Color: $00408080; Name: ''),
    (Color: $00808080; Name: 'Gray'),
    (Color: $00808040; Name: ''),
    (Color: $00C0C0C0; Name: 'Silver'),
    (Color: $00400040; Name: ''),
    (Color: $00FFFFFF; Name: 'White')
  );
  {$endregion Colors_Standard48}    
         
           
  {$region ' ------ Colors_Web_White ------ '}
  Colors_Web_White: array[0..16] of TColorArrayItem = (
    (Color: $00FFFFFF; Name: 'White'),
    (Color: $00FAFAFF; Name: 'Snow'),
    (Color: $00F0FFF0; Name: 'Honeydew'),
    (Color: $00FAFFF5; Name: 'MintCream'),
    (Color: $00FFFFF0; Name: 'Azure'),
    (Color: $00FFF8F0; Name: 'AliceBlue'),
    (Color: $00FFF8F8; Name: 'GhostWhite'),
    (Color: $00F5F5F5; Name: 'WhiteSmoke'),
    (Color: $00EEF5FF; Name: 'Seashell'),
    (Color: $00DCF5F5; Name: 'Beige'),
    (Color: $00E6F5FD; Name: 'OldLace'),
    (Color: $00F0FAFF; Name: 'FloralWhite'),
    (Color: $00F0FFFF; Name: 'Ivory'),
    (Color: $00D7EBFA; Name: 'AntiqueWhite'),
    (Color: $00E6F0FA; Name: 'Linen'),
    (Color: $00F5F0FF; Name: 'LavenderBlush'),
    (Color: $00E1E4FF; Name: 'MistyRose')
  ); 
  {$endregion Colors_Web_White}    
  
  
  {$region ' ------ Colors_Web_Red ------ '}
  Colors_Web_Red: array[0..8] of TColorArrayItem = (
    (Color: $007AA0FF; Name: 'LightSalmon'),
    (Color: $007280FA; Name: 'Salmon'),
    (Color: $007A96E9; Name: 'DarkSalmon'),
    (Color: $008080F0; Name: 'LightCoral'),
    (Color: $005C5CCD; Name: 'IndianRed'),
    (Color: $005C5CCD; Name: 'Crimson'),
    (Color: $002222B2; Name: 'FireBrick'),
    (Color: $0000008B; Name: 'DarkRed'),
    (Color: $000000FF; Name: 'Red')
  );
  {$endregion Colors_Web_Red}
                  
  
  {$region ' ------ Colors_Web_PurpleVioletMagenta ------ '}  
  Colors_Web_PurpleVioletMagenta: array[0..17] of TColorArrayItem = (
    (Color: $00FAE6E6; Name: 'Lavender'),
    (Color: $00D8BFD8; Name: 'Thistle'),
    (Color: $00DDA0DD; Name: 'Plum'),
    (Color: $00EE82EE; Name: 'Violet'),
    (Color: $00D670DA; Name: 'Orchid'),
    (Color: $00FF00FF; Name: 'Fuchsia'),
    (Color: $00FF00FF; Name: 'Magenta'),
    (Color: $00D355BA; Name: 'MediumOrchid'),
    (Color: $00DB7093; Name: 'MediumPurple'),
    (Color: $00E22B8A; Name: 'BlueViolet'),
    (Color: $00D30094; Name: 'DarkViolet'),
    (Color: $00CC3299; Name: 'DarkOrchid'),
    (Color: $008B008B; Name: 'DarkMagenta'),
    (Color: $00800080; Name: 'Purple'),
    (Color: $0082004B; Name: 'Indigo'),
    (Color: $008B3D48; Name: 'DarkSlateBlue'),
    (Color: $00CD5A6A; Name: 'SlateBlue'),
    (Color: $00E26375; Name: 'MediumSlateBlue')
  );
  {$endregion Colors_Web_PurpleVioletMagenta}
                                              
  
  {$region ' ------ Colors_Web_Pink ------ '}  
  Colors_Web_Pink: array[0..5] of TColorArrayItem = (
    (Color: $00CBC0FF; Name: 'Pink'),
    (Color: $00C1B6FF; Name: 'LightPink'),
    (Color: $00B469FF; Name: 'HotPink'),
    (Color: $009314FF; Name: 'DeepPink'),
    (Color: $009370DB; Name: 'PaleVioletRed'),
    (Color: $008515C7; Name: 'MediumVioletRed')
  );
  {$endregion Colors_Web_Pink}
                       
  
  {$region ' ------ Colors_Web_Orange ------ '}  
  Colors_Web_Orange: array[0..4] of TColorArrayItem = (
    (Color: $000045FF; Name: 'OrangeRed'),
    (Color: $004763FF; Name: 'Tomato'),
    (Color: $00507FFF; Name: 'Coral'),
    (Color: $00008CFF; Name: 'DarkOrange'),
    (Color: $0000A5FF; Name: 'Orange')
  );
  {$endregion Colors_Web_Orange}  
  
  
  {$region ' ------ Colors_Web_Green ------ '}  
  Colors_Web_Green: array[0..19] of TColorArrayItem = (
    (Color: $002F6B55; Name: 'DarkOliveGreen'),
    (Color: $00008080; Name: 'Olive'),
    (Color: $00238E6B; Name: 'OliveDrab'),
    (Color: $0032CD9A; Name: 'YellowGreen'),
    (Color: $0032CD32; Name: 'LimeGreen'),
    (Color: $0000FF00; Name: 'Lime'),
    (Color: $0000FC7C; Name: 'LawnGreen'),
    (Color: $0000FF7F; Name: 'Chartreuse'),
    (Color: $002FFFAD; Name: 'GreenYellow'),
    (Color: $007FFF00; Name: 'SpringGreen'),
    (Color: $009AFA00; Name: 'MediumSpringGreen'),
    (Color: $0090EE90; Name: 'LightGreen'),
    (Color: $0098FB98; Name: 'PaleGreen'),
    (Color: $008FBC8F; Name: 'DarkSeaGreen'),
    (Color: $00AACD66; Name: 'MediumAquamarine'),
    (Color: $0071B33C; Name: 'MediumSeaGreen'),
    (Color: $00578B2E; Name: 'SeaGreen'),
    (Color: $00228B22; Name: 'ForestGreen'),
    (Color: $00008000; Name: 'Green'),
    (Color: $00006400; Name: 'DarkGreen')
  );
  {$endregion Colors_Web_Green} 
  
  
  {$region ' ------ Colors_Web_GrayBlack ------ '}  
  Colors_Web_GrayBlack: array[0..9] of TColorArrayItem = (
    (Color: $00DCDCDC; Name: 'Gainsboro'),
    (Color: $00D3D3D3; Name: 'LightGrey'),
    (Color: $00C0C0C0; Name: 'Silver'),
    (Color: $00A9A9A9; Name: 'DarkGray'),
    (Color: $00808080; Name: 'Gray'),
    (Color: $00696969; Name: 'DimGray'),
    (Color: $00998877; Name: 'LightSlateGray'),
    (Color: $00908070; Name: 'SlateGray'),
    (Color: $004F4F2F; Name: 'DarkSlateGray'),
    (Color: $00000000; Name: 'Black')
  );
  {$endregion Colors_Web_GrayBlack}   
  
  
  {$region ' ------ Colors_Web_Cyan ------ '}  
  Colors_Web_Cyan: array[0..11] of TColorArrayItem = (
    (Color: $00FFFF00; Name: 'Aqua'),
    (Color: $00FFFF00; Name: 'Cyan'),
    (Color: $00FFFFE0; Name: 'LightCyan'),
    (Color: $00EEEEAF; Name: 'PaleTurquoise'),
    (Color: $00D4FF7F; Name: 'Aquamarine'),
    (Color: $00D0E040; Name: 'Turquoise'),
    (Color: $00CCD148; Name: 'MediumTurquoise'),
    (Color: $00D1CE00; Name: 'DarkTurquoise'),
    (Color: $00AAB220; Name: 'LightSeaGreen'),
    (Color: $00A09E5F; Name: 'CadetBlue'),
    (Color: $008B8B00; Name: 'DarkCyan'),
    (Color: $00808000; Name: 'Teal')
  );
  {$endregion Colors_Web_Cyan}   
  
  
  {$region ' ------ Colors_Web_Brown ------ '}  
  Colors_Web_Brown: array[0..16] of TColorArrayItem = (
    (Color: $00DCF8FF; Name: 'Cornsilk'),
    (Color: $00CDEBFF; Name: 'BlanchedAlmond'),
    (Color: $00C4E4FF; Name: 'Bisque'),
    (Color: $00ADDEFF; Name: 'NavajoWhite'),
    (Color: $00B3DEF5; Name: 'Wheat'),
    (Color: $0087B8DE; Name: 'BurlyWood'),
    (Color: $008CB4D2; Name: 'Tan'),
    (Color: $008F8FBC; Name: 'RosyBrown'),
    (Color: $0060A4F4; Name: 'SandyBrown'),
    (Color: $0020A5DA; Name: 'Goldenrod'),
    (Color: $000B86B8; Name: 'DarkGoldenrod'),
    (Color: $003F85CD; Name: 'Peru'),
    (Color: $001E69D2; Name: 'Chocolate'),
    (Color: $0013458B; Name: 'SaddleBrown'),
    (Color: $002D52A0; Name: 'Sienna'),
    (Color: $002A2AA5; Name: 'Brown'),
    (Color: $00000080; Name: 'Maroon')
  );
  {$endregion Colors_Web_Brown} 
  
  
  {$region ' ------ Colors_Web_Blue ------ '}  
  Colors_Web_Blue: array[0..14] of TColorArrayItem = (
    (Color: $00DEC4B0; Name: 'LightSteelBlue'),
    (Color: $00E6E0B0; Name: 'PowderBlue'),
    (Color: $00E6D8AD; Name: 'LightBlue'),
    (Color: $00EBCE87; Name: 'SkyBlue'),
    (Color: $00FACE87; Name: 'LightSkyBlue'),
    (Color: $00FFBF00; Name: 'DeepSkyBlue'),
    (Color: $00FF901E; Name: 'DodgerSkyBlue'),
    (Color: $00ED9564; Name: 'CornflowerBlue'),
    (Color: $00B48246; Name: 'SteelBlue'),
    (Color: $00E16941; Name: 'RoyalBlue'),
    (Color: $00FF0000; Name: 'Blue'),
    (Color: $00CD0000; Name: 'MediumBlue'),
    (Color: $008B0000; Name: 'DarkBlue'),
    (Color: $00800000; Name: 'Navy'),
    (Color: $00701919; Name: 'MidnightBlue')
  );
  {$endregion Colors_Web_Blue} 
  
  
  {$region ' ------ Colors_Web_Yellow ------ '}  
  Colors_Web_Yellow: array[0..10] of TColorArrayItem = (
    (Color: $0000FFFF; Name: 'Yellow'),
    (Color: $00E0FFFF; Name: 'LightYellow'),
    (Color: $00CDFAFF; Name: 'LemonChiffon'),
    (Color: $00D2FAFA; Name: 'LightGoldenrodYellow'),
    (Color: $00D5EFFF; Name: 'PapayaWhip'),
    (Color: $00B5E4FF; Name: 'Moccasin'),
    (Color: $00B9DAFF; Name: 'PeachPuff'),
    (Color: $00AAE8EE; Name: 'PaleGoldenrod'),
    (Color: $008CE6F0; Name: 'Khaki'),
    (Color: $006BB7BD; Name: 'DarkKhaki'),
    (Color: $0000D7FF; Name: 'Gold')
  );
  {$endregion Colors_Web_Yellow}  
                  
  
  {$region ' ------ Colors_Web_216_Safe ------ '}  
  Colors_Web_216_Safe: array[0..215] of TColorArrayItem = (

    // 1 - 10
    (Color: $00000000; Name: 'Black'),
    (Color: $00000033; Name: ''),
    (Color: $00000066; Name: ''),
    (Color: $00000099; Name: ''),
    (Color: $000000CC; Name: ''),
    (Color: $000000FF; Name: 'Red'),
    (Color: $00330000; Name: ''),
    (Color: $00330033; Name: ''),
    (Color: $00330066; Name: ''),
    (Color: $00330099; Name: ''),

    // 11 - 20
    (Color: $003300CC; Name: ''),
    (Color: $003300FF; Name: ''),
    (Color: $00660000; Name: ''),
    (Color: $00660033; Name: ''),
    (Color: $00660066; Name: ''),
    (Color: $00660099; Name: ''),
    (Color: $006600CC; Name: ''),
    (Color: $006600FF; Name: ''),
    (Color: $00990000; Name: ''),
    (Color: $00990033; Name: ''),

    // 21 - 30
    (Color: $00990066; Name: ''),
    (Color: $00990099; Name: ''),
    (Color: $009900CC; Name: ''),
    (Color: $009900FF; Name: ''),
    (Color: $00CC0000; Name: ''),
    (Color: $00CC0033; Name: ''),
    (Color: $00CC0066; Name: ''),
    (Color: $00CC0099; Name: ''),
    (Color: $00CC00CC; Name: ''),
    (Color: $00CC00FF; Name: ''),

    // 31 - 40
    (Color: $00FF0000; Name: 'Blue'),
    (Color: $00FF0033; Name: ''),
    (Color: $00FF0066; Name: ''),
    (Color: $00FF0099; Name: ''),
    (Color: $00FF00CC; Name: ''),
    (Color: $00FF00FF; Name: 'Fuchsia'),
    (Color: $00003300; Name: ''),
    (Color: $00003333; Name: ''),
    (Color: $00003366; Name: ''),
    (Color: $00003399; Name: ''),

    // 41 - 50
    (Color: $000033CC; Name: ''),
    (Color: $000033FF; Name: ''),
    (Color: $00333300; Name: ''),
    (Color: $00333333; Name: ''),
    (Color: $00333366; Name: ''),
    (Color: $00333399; Name: ''),
    (Color: $003333CC; Name: ''),
    (Color: $003333FF; Name: ''),
    (Color: $00663300; Name: ''),
    (Color: $00663333; Name: ''),

    // 51 - 60
    (Color: $00663366; Name: ''),
    (Color: $00663399; Name: ''),
    (Color: $006633CC; Name: ''),
    (Color: $006633FF; Name: ''),
    (Color: $00993300; Name: ''),
    (Color: $00993333; Name: ''),
    (Color: $00993366; Name: ''),
    (Color: $00993399; Name: ''),
    (Color: $009933CC; Name: ''),
    (Color: $009933FF; Name: ''),

    // 61 - 70
    (Color: $00CC3300; Name: ''),
    (Color: $00CC3333; Name: ''),
    (Color: $00CC3366; Name: ''),
    (Color: $00CC3399; Name: ''),
    (Color: $00CC33CC; Name: ''),
    (Color: $00CC33FF; Name: ''),
    (Color: $00FF3300; Name: ''),
    (Color: $00FF3333; Name: ''),
    (Color: $00FF3366; Name: ''),
    (Color: $00FF3399; Name: ''),

    // 71 - 80
    (Color: $00FF33CC; Name: ''),
    (Color: $00FF33FF; Name: ''),
    (Color: $00006600; Name: ''),
    (Color: $00006633; Name: ''),
    (Color: $00006666; Name: ''),
    (Color: $00006699; Name: ''),
    (Color: $000066CC; Name: ''),
    (Color: $000066FF; Name: ''),
    (Color: $00336600; Name: ''),
    (Color: $00336633; Name: ''),

    // 81 - 90
    (Color: $00336666; Name: ''),
    (Color: $00336699; Name: ''),
    (Color: $003366CC; Name: ''),
    (Color: $003366FF; Name: ''),
    (Color: $00666600; Name: ''),
    (Color: $00666633; Name: ''),
    (Color: $00666666; Name: ''),
    (Color: $00666699; Name: ''),
    (Color: $006666CC; Name: ''),
    (Color: $006666FF; Name: ''),

    // 91 - 100
    (Color: $00996600; Name: ''),
    (Color: $00996633; Name: ''),
    (Color: $00996666; Name: ''),
    (Color: $00996699; Name: ''),
    (Color: $009966CC; Name: ''),
    (Color: $009966FF; Name: ''),
    (Color: $00CC6600; Name: ''),
    (Color: $00CC6633; Name: ''),
    (Color: $00CC6666; Name: ''),
    (Color: $00CC6699; Name: ''),

    // 101 - 110
    (Color: $00CC66CC; Name: ''),
    (Color: $00CC66FF; Name: ''),
    (Color: $00FF6600; Name: ''),
    (Color: $00FF6633; Name: ''),
    (Color: $00FF6666; Name: ''),
    (Color: $00FF6699; Name: ''),
    (Color: $00FF66CC; Name: ''),
    (Color: $00FF66FF; Name: ''),
    (Color: $00009900; Name: ''),
    (Color: $00009933; Name: ''),

    // 111 - 120
    (Color: $00009966; Name: ''),
    (Color: $00009999; Name: ''),
    (Color: $000099CC; Name: ''),
    (Color: $000099FF; Name: ''),
    (Color: $00339900; Name: ''),
    (Color: $00339933; Name: ''),
    (Color: $00339966; Name: ''),
    (Color: $00339999; Name: ''),
    (Color: $003399CC; Name: ''),
    (Color: $003399FF; Name: ''),

    // 121 - 130
    (Color: $00669900; Name: ''),
    (Color: $00669933; Name: ''),
    (Color: $00669966; Name: ''),
    (Color: $00669999; Name: ''),
    (Color: $006699CC; Name: ''),
    (Color: $006699FF; Name: ''),
    (Color: $00999900; Name: ''),
    (Color: $00999933; Name: ''),
    (Color: $00999966; Name: ''),
    (Color: $00999999; Name: ''),

    // 131 - 140
    (Color: $009999CC; Name: ''),
    (Color: $009999FF; Name: ''),
    (Color: $00CC9900; Name: ''),
    (Color: $00CC9933; Name: ''),
    (Color: $00CC9966; Name: ''),
    (Color: $00CC9999; Name: ''),
    (Color: $00CC99CC; Name: ''),
    (Color: $00CC99FF; Name: ''),
    (Color: $00FF9900; Name: ''),
    (Color: $00FF9933; Name: ''),

    // 141 - 150
    (Color: $00FF9966; Name: ''),
    (Color: $00FF9999; Name: ''),
    (Color: $00FF99CC; Name: ''),
    (Color: $00FF99FF; Name: ''),
    (Color: $0000CC00; Name: ''),
    (Color: $0000CC33; Name: ''),
    (Color: $0000CC66; Name: ''),
    (Color: $0000CC99; Name: ''),
    (Color: $0000CCCC; Name: ''),
    (Color: $0000CCFF; Name: ''),

    // 151 - 160
    (Color: $0033CC00; Name: ''),
    (Color: $0033CC33; Name: ''),
    (Color: $0033CC66; Name: ''),
    (Color: $0033CC99; Name: ''),
    (Color: $0033CCCC; Name: ''),
    (Color: $0033CCFF; Name: ''),
    (Color: $0066CC00; Name: ''),
    (Color: $0066CC33; Name: ''),
    (Color: $0066CC66; Name: ''),
    (Color: $0066CC99; Name: ''),

    // 161 - 170
    (Color: $0066CCCC; Name: ''),
    (Color: $0066CCFF; Name: ''),
    (Color: $0099CC00; Name: ''),
    (Color: $0099CC33; Name: ''),
    (Color: $0099CC66; Name: ''),
    (Color: $0099CC99; Name: ''),
    (Color: $0099CCCC; Name: ''),
    (Color: $0099CCFF; Name: ''),
    (Color: $00CCCC00; Name: ''),
    (Color: $00CCCC33; Name: ''),

    // 171 - 180
    (Color: $00CCCC66; Name: ''),
    (Color: $00CCCC99; Name: ''),
    (Color: $00CCCCCC; Name: ''),
    (Color: $00CCCCFF; Name: ''),
    (Color: $00FFCC00; Name: ''),
    (Color: $00FFCC33; Name: ''),
    (Color: $00FFCC66; Name: ''),
    (Color: $00FFCC99; Name: ''),
    (Color: $00FFCCCC; Name: ''),
    (Color: $00FFCCFF; Name: ''),

    // 181 - 190
    (Color: $0000FF00; Name: 'Lime'),
    (Color: $0000FF33; Name: ''),
    (Color: $0000FF66; Name: ''),
    (Color: $0000FF99; Name: ''),
    (Color: $0000FFCC; Name: ''),
    (Color: $0000FFFF; Name: 'Yellow'),
    (Color: $0033FF00; Name: ''),
    (Color: $0033FF33; Name: ''),
    (Color: $0033FF66; Name: ''),
    (Color: $0033FF99; Name: ''),

    // 191 - 200
    (Color: $0033FFCC; Name: ''),
    (Color: $0033FFFF; Name: ''),
    (Color: $0066FF00; Name: ''),
    (Color: $0066FF33; Name: ''),
    (Color: $0066FF66; Name: ''),
    (Color: $0066FF99; Name: ''),
    (Color: $0066FFCC; Name: ''),
    (Color: $0066FFFF; Name: ''),
    (Color: $0099FF00; Name: ''),
    (Color: $0099FF33; Name: ''),

    // 201 - 210
    (Color: $0099FF66; Name: ''),
    (Color: $0099FF99; Name: ''),
    (Color: $0099FFCC; Name: ''),
    (Color: $0099FFFF; Name: ''),
    (Color: $00CCFF00; Name: ''),
    (Color: $00CCFF33; Name: ''),
    (Color: $00CCFF66; Name: ''),
    (Color: $00CCFF99; Name: ''),
    (Color: $00CCFFCC; Name: ''),
    (Color: $00CCFFFF; Name: ''),

    // 211 - 216
    (Color: $00FFFF00; Name: 'Aqua'),
    (Color: $00FFFF33; Name: ''),
    (Color: $00FFFF66; Name: ''),
    (Color: $00FFFF99; Name: ''),
    (Color: $00FFFFCC; Name: ''),
    (Color: $00FFFFFF; Name: 'White')
  );
  {$endregion Colors_Web_216_Safe}
  
  
  
  
implementation



end.


