;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
PRO VIIRS_NightLights_UI_Extensions_Init
  COMPILE_OPT idl2, hidden

  ; Get the current ENVI session
  e = ENVI(/CURRENT)

  ; Add the custom task as an extension to the ENVI toolbox
  e.AddExtension, 'VIIRS NightLights 2CMV', $
    'VIIRS_NightLights_UI', PATH='GeoAgent', UVALUE='viirs_dnb_change_detection'
  e.AddExtension, 'NightLight HeatMap', $
    'VIIRS_NightLights_UI', PATH='GeoAgent', UVALUE='light_heatmap_series'
  e.AddExtension, 'NightLight Transient Map', $
    'VIIRS_NightLights_UI', PATH='GeoAgent', UVALUE='light_transient_series'

END

;*************************************************************************
PRO VIIRS_NightLights_UI, event
  COMPILE_OPT idl2, hidden

  ; Error handler
  CATCH, errorStatus
  IF (errorStatus ne 0) THEN BEGIN
    CATCH, /CANCEL
    PRINT, !ERROR_STATE.MSG
    RETURN
  ENDIF

  ; open ENVI object
  e = ENVI(/CURRENT)
  IF ~OBJ_VALID(e) THEN BEGIN
    e = ENVI(/HEADLESS)
    closeOnExit = 1B
  ENDIF ELSE closeOnExit = 0B

  ; Get the UI object
  UI = e.UI
  WIDGET_CONTROL, event.ID, GET_UVALUE = uvalue

  ;Get the task
  CASE uvalue OF
    'viirs_dnb_change_detection': Task = ENVITask('NightLights_2CMV_ChangeDetection')
    'light_heatmap_series': Task = ENVITask('NightLights_HeatMap_ChangeDetection')
    'light_transient_series': Task = ENVITask('NightLights_Transient_ChangeDetection')
    ELSE: 
  ENDCASE

  ;As of ENVI 5.6, there is the ENVIUI::RunTask method that displays a dialog where
  ;users can select parameters for given ENVITask (like SelectTaskParameters), but
  ;this method will 1) auto run task after user clicks OK; 2) lets user choose to 
  ;run task on an ENVI Server and be managed by GSF.
  ;;
  dynamicUI = e.UI.SelectTaskParameters(Task)
  IF (dynamicUI NE 'OK') THEN RETURN
  Task.Execute

  ;e.UI.RunTask, Task
 
;  ; Add the result to the Data Manager
;  DataColl = e.Data
;  DataColl.Add, Task.OUTPUT_RASTER   ;oOutput
;
;  ; Display the result
;  view = e.GetView()
;  layer = view.CreateLayer(Task.OUTPUT_RASTER)
;  view.Zoom, /FULL_EXTENT

  RETURN
END

