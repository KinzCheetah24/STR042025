
with Kernel.Serial_Output; use Kernel.Serial_Output;
with Ada.Real_Time; use Ada.Real_Time;
with System; use System;

with Tools; use Tools;
with devicesFSS_V1; use devicesFSS_V1;

-- NO ACTIVAR ESTE PAQUETE MIENTRAS NO SE TENGA PROGRAMADA LA INTERRUPCION
-- Packages needed to generate button interrupts       
-- with Ada.Interrupts.Names;
-- with Button_Interrupt; use Button_Interrupt;

package body fss is

  ----------------------------------------------------------------------
  ------------- procedure exported 
  ----------------------------------------------------------------------
  procedure Background is
  begin
    loop
      null;
    end loop;
  end Background;
  ----------------------------------------------------------------------

  -----------------------------------------------------------------------
  ------------- declaration of protected objects 
  -----------------------------------------------------------------------

  -- Aqui se declaran los objetos protegidos para los datos compartidos  


  ----------------------------------------------------------------------
  ------------- procedimientos para probar los dispositivos 
  ------------- SE DEBERÁN QUITAR PARA EL PROYECTO
  ----------------------------------------------------------------------
  procedure Prueba_Velocidad_Distancia; 
  procedure Prueba_Altitud_Joystick; 
  procedure Prueba_Sensores_Piloto;

  procedure Mode (modeInput : in String);
  procedure PositionAltitude;
  procedure Speed;
  procedure Collision;
  procedure Display;

  procedure Mode (modeInput : in String) is
  begin
    if (modeInput = "Manual") then
      ManualMode;
    elsif (modeInput = "Automatic") then
      AutomaticMode;
    end if;
  end Mode;

  procedure PositionAltitude is
  begin
  end PositionAltitude;

  procedure Speed is

    Current_Power: Power_Samples_Type := 0;
    Current_Pp: PilotPresence_Samples_Type := 1;
    Current_J: Joystick_Samples_Type := (0,0);

    Calculated_S: Speed_Samples_type := 0;
    Target_Pitch: Pitch_Samples_Type := 0;
    Target_Roll: Roll_Samples_Type := 0; 
    

  begin

    Read_Power (Current_Power);
    Calculated_S := Speed_Samples_Type (float (Current_Power) * 1.2);

    Read_Joystick (Current_J);
    Target_Pitch := Pitch_Samples_Type (Current_J(x));
    Target_Roll := Roll_Samples_Type (Current_J(y));

    if (Target_Pitch > 0 and Target_Roll > 0) then
      Calculated_S := Calculated_S + (Speed_Samples_Type (200));
    elsif (Target_Pitch > 0) then
      Calculated_S := Calculated_S + (Speed_Samples_Type (150));
    elsif (Target_Roll > 0) then
      Calculated_S := Calculated_S + (Speed_Samples_Type (100));
    end if;

    if (Calculated_S >= 1000) then
      Set_Speed(Speed_Samples_Type (1000));
      Light_2(On);
    elsif (Calculated_S <= 300) then
      Set_Speed(Speed_Samples_Type (300));
      Light_2(On);
    else
      Set_Speed(Calculated_S);
    end if;

  end Speed;

  procedure PositionAltitude is

    Current_J: Joystick_Samples_Type := (0,0);
    Target_Pitch: Pitch_Samples_Type := 0;
    Target_Roll: Roll_Samples_Type := 0; 
    Aircraft_Pitch: Pitch_Samples_Type; 
    Aircraft_Roll: Roll_Samples_Type;
    
    Current_A: Altitude_Samples_Type := Altitude_Samples_Type(8000);

  begin

    Read_Joystick (Current_J);

    Target_Pitch := Pitch_Samples_Type (Current_J(x));
    Target_Roll := Roll_Samples_Type (Current_J(y));

    if (Target_Pitch > 30) then
      Target_Pitch := 30;
    elsif (Target_Pitch < -30) then
      Target_Pitch := -30;
    end if;

    if (Target_Roll > 45) then
      Target_Roll := 45;
    elsif (Target_Roll < -45) then
      Target_Roll := -45;
    end if;

    Current_A := Read_Altitude;

    if (Current_A <= 2500) then
      Light_1(On);

      if (Current_A <= 2000) then
        Set_Aircraft_Pitch (Pitch_Samples_Type (0));
        Set_Aircraft_Roll (Roll_Samples_Type (0));
      else
        Set_Aircraft_Pitch (Target_Pitch);
        Set_Aircraft_Roll (Target_Roll);

        if (Target_Roll > 35 or Target_Roll < -35) then
          Display_Message ("Roll higher than +35/-35");
        end if;
      end if;
    elsif (Current_A >= 9500) then
      Light_1(On);

      if (Current_A >= 10000) then
        Set_Aircraft_Pitch (Pitch_Samples_Type (0));
        Set_Aircraft_Roll (Roll_Samples_Type (0));
      else
        Set_Aircraft_Pitch (Target_Pitch);
        Set_Aircraft_Roll (Target_Roll);

        if (Target_Roll > 35 or Target_Roll < -35) then
          Display_Message ("Roll higher than +35/-35");
        end if;
      end if;
    
    else
      Set_Aircraft_Pitch (Target_Pitch);
      Set_Aircraft_Roll (Target_Roll);

      if (Target_Roll > 35 or Target_Roll < -35) then
          Display_Message ("Roll higher than +35/-35");
        end if;
    end if;

  end PositionAltitude;
  
  Procedure Collision is
    
    Current_Distance: Distance_Samples_Type := 0;
    Current_Speed: Speed_Samples_Type := 0;
    Time_Until_Collision: float := 0;
    Current_Pp: PilotPresence_Samples_Type := 1;
    Current_Light: Light_Samples_Type := 0;
  
  begin
    
    Read_Distance(Current_Distance);
    Current_Speed := Read_Speed;
    Current_Pp := Read_PilotPresence;
    Get_Light(Current_Light);
    
    Time_Until_Collision := Float(Current_Distance) / (Float(Current_Speed) * 1000.0 / 3600.0);
    
    if Current_Pp = 0 or Current_Light < 500 then
      if Time_Until_Collision < 15 then
        Alarm(4);
      elsif Time_Until_Collision < 10 then
        "Desvío Automatico"
      end if;
    elsif Time_Until_Collision < 10 then
        Alarm(4);
    elsif Time_Until_Collision < 5 then
      "Desvío Automatico"
    end if;
    
  end Collision;

  procedure Display is

    Current_A: Altitude_Samples_Type := Altitude_Samples_Type(8000);
    Current_Power: Power_Samples_Type := 0;
    Current_J: Joystick_Samples_Type := (0,0);
    Aircraft_Pitch: Pitch_Samples_Type; 
    Aircraft_Roll: Roll_Samples_Type;

  begin

    Current_A := Read_Altitude;
    Read_Power (Current_Pw);
    Read_Joystick (Current_J);
    Aircraft_Pitch := Read_Pitch;
    Aircraft_Roll := Read_Roll;

    Display_Altitude (Current_A);
    Display_Pilot_Power(Current_Power);
    Display_Speed(Speed_Samples_Type (float (Current_Power) * 1.2));
    Display_Joystick (Current_J);
    Display_Pitch (Aircraft_Pitch);
    Display_Roll (Aircraft_Roll);
    Display_Message("Message for the Pilot");

  end Display;

  -----------------------------------------------------------------------
  ------------- declaration of tasks 
  -----------------------------------------------------------------------

  -- Aqui se declaran las tareas que forman el STR

  task A is
    pragma priority (5);
  end A;


  -----------------------------------------------------------------------
  ------------- body of tasks 
  -----------------------------------------------------------------------
  
  -- Aqui se escriben los cuerpos de las tareas 
  
  task body A is 
  begin
    null;
  end A;

begin
  null;
end fss;