unit StartSelector;

{$mode objfpc}{$H+}

interface

uses
   Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

			{ TStartSelectForm }

   TStartSelectForm = class(TForm)
						Button1: TButton;
						StartSelector: TComboBox;
   private
      { private declarations }
   public
      { public declarations }
   end;

var
   StartSelectForm: TStartSelectForm;

implementation

{$R *.lfm}

end.

