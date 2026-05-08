module Main where

import System.Random   (newStdGen, randomR, StdGen)
import Data.List       (intercalate, maximumBy, groupBy, sortBy)
import Data.Ord        (comparing, Down(..))
import Data.Char       (toLower)

-- ============================================================
-- TIPO DE DATO: Evento
-- Representa una acción ocurrida en la plataforma de comercio.
-- Campos:
--   eventoId  : identificador único del evento (entero)
--   categoria : tipo de evento (texto)
--   valor     : monto asociado al evento (decimal)
--   timestamp : momento en que ocurrió el evento (entero)
-- ============================================================
data Evento = Evento
    { eventoId  :: Int
    , categoria :: String
    , valor     :: Float
    , timestamp :: Int
    } deriving (Show, Eq)

-- ============================================================
-- CATEGORÍAS DISPONIBLES
-- Lista de todas las categorías posibles para un evento.
-- ============================================================
categorias :: [String]
categorias = ["visualizacion", "apartado", "compra", "devolucion", "seguimiento"]

-- ============================================================
-- FUNCIÓN: generarEvento
-- Genera un único evento aleatorio usando un generador.
-- Entrada: gen -> generador de números aleatorios
-- Salida:  (Evento, StdGen) -> evento generado y nuevo generador
-- ============================================================
generarEvento :: StdGen -> (Evento, StdGen)
generarEvento gen =
    let (eid, g1) = randomR (0,          9000000)    gen
        (cat, g2) = randomR (0,          4)          g1
        (val, g3) = randomR (500.0,      75000.0)    g2
        (ts,  g4) = randomR (1746000000, 1809000000) g3
        catNombre = categorias !! cat
        evento    = Evento eid catNombre val ts
    in  (evento, g4)

-- ============================================================
-- FUNCIÓN: generarEventos
-- Genera una lista de N eventos aleatorios recursivamente.
-- Entrada: n -> cantidad, gen -> generador
-- Salida:  ([Evento], StdGen) -> lista de eventos y generador final
-- ============================================================
generarEventos :: Int -> StdGen -> ([Evento], StdGen)
generarEventos 0 gen = ([], gen)
generarEventos n gen =
    let (evento, g1)    = generarEvento gen
        (resto, gFinal) = generarEventos (n - 1) g1
    in  (evento : resto, gFinal)

-- ============================================================
-- FUNCIÓN: alimentarSistema
-- Genera entre 10 y 25 eventos nuevos y los agrega a la lista.
-- Entrada: eventos -> lista actual, gen -> generador
-- Salida:  ([Evento], StdGen) -> lista actualizada y nuevo generador
-- ============================================================
alimentarSistema :: [Evento] -> StdGen -> ([Evento], StdGen)
alimentarSistema eventos gen =
    let (cantidad, g1)     = randomR (10, 25) gen
        (nuevos,   gFinal) = generarEventos cantidad g1
    in  (eventos ++ nuevos, gFinal)

-- ============================================================
-- FUNCIÓN: timestampAFecha
-- Convierte un timestamp Unix a (año, mes, dia, diaSemana).
-- Usa cálculo de calendario para obtener la fecha.
-- Entrada: ts -> timestamp en segundos
-- Salida:  (año, mes, día, díaSemana) donde díaSemana 0=Lun..6=Dom
-- ============================================================
timestampAFecha :: Int -> (Int, Int, Int, Int)
timestampAFecha ts =
    let dias       = ts `div` 86400
        diaSemana  = (dias + 3) `mod` 7
        -- Algoritmo para convertir días desde epoch a fecha
        z          = dias + 719468
        era        = (if z >= 0 then z else z - 146096) `div` 146097
        doe        = z - era * 146097
        yoe        = (doe - doe `div` 1460 + doe `div` 36524 - doe `div` 146096) `div` 365
        y          = yoe + era * 400
        doy        = doe - (365 * yoe + yoe `div` 4 - yoe `div` 100)
        mp         = (5 * doy + 2) `div` 153
        d          = doy - (153 * mp + 2) `div` 5 + 1
        m          = if mp < 10 then mp + 3 else mp - 9
        anio       = if m <= 2 then y + 1 else y
    in  (anio, m, d, diaSemana)

-- ============================================================
-- FUNCIÓN: nombreDia
-- Convierte número de día (0-6) a nombre en español.
-- Entrada: n -> número de día (0=Lunes, 6=Domingo)
-- Salida:  nombre del día como String
-- ============================================================
nombreDia :: Int -> String
nombreDia n = ["Lunes","Martes","Miércoles","Jueves","Viernes","Sábado","Domingo"] !! n

-- ============================================================
-- FUNCIÓN: nombreMes
-- Convierte número de mes (1-12) a nombre en español.
-- Entrada: n -> número de mes
-- Salida:  nombre del mes como String
-- ============================================================
nombreMes :: Int -> String
nombreMes n = ["","Enero","Febrero","Marzo","Abril","Mayo","Junio",
               "Julio","Agosto","Septiembre","Octubre","Noviembre","Diciembre"] !! n

-- ============================================================
-- FUNCIÓN: validarMonto
-- Verifica que un evento tenga monto mayor a cero.
-- Entrada: e -> evento a validar
-- Salida:  True si el monto es válido, False si no
-- ============================================================
validarMonto :: Evento -> Bool
validarMonto e = valor e > 0

-- ============================================================
-- FUNCIÓN: reportarInconsistencias
-- Filtra y muestra eventos con montos inválidos.
-- Entrada: eventos -> lista completa de eventos
-- Salida:  IO () imprime los eventos inconsistentes
-- ============================================================
reportarInconsistencias :: [Evento] -> IO ()
reportarInconsistencias eventos = do
    let invalidos = filter (not . validarMonto) eventos
    if null invalidos
        then putStrLn "  No se encontraron inconsistencias."
        else do
            putStrLn $ "  Se encontraron " ++ show (length invalidos) ++ " evento(s) con monto inválido:"
            mapM_ (\e -> putStrLn $ "    ID: " ++ show (eventoId e) ++ " | Valor: " ++ show (valor e)) invalidos

-- ============================================================
-- TRANSFORMACIÓN: etiquetarAltoValor
-- Marca con "[ALTO VALOR]" los eventos cuyo monto supera
-- el promedio de su categoría.
-- Entrada: eventos -> lista de eventos válidos
-- Salida:  lista de Strings con el resultado etiquetado
-- ============================================================
etiquetarAltoValor :: [Evento] -> [String]
etiquetarAltoValor eventos =
    map etiquetar eventos
  where
    -- Calcula el promedio de una categoría específica
    promedioCat cat =
        let delCat = filter (\e -> categoria e == cat) eventos
            total  = sum (map valor delCat)
        in  if null delCat then 0 else total / fromIntegral (length delCat)
    -- Etiqueta cada evento comparando con el promedio de su categoría
    etiquetar e =
        let prom = promedioCat (categoria e)
            tag  = if valor e > prom then " [ALTO VALOR]" else ""
        in  "ID:" ++ show (eventoId e)
            ++ " | Cat:" ++ categoria e
            ++ " | Val:" ++ show (valor e)
            ++ " | Prom.Cat:" ++ show prom
            ++ tag

-- ============================================================
-- ANÁLISIS: promedioPorCategoriaAnio
-- Calcula el promedio de monto por categoría agrupado por año.
-- Entrada: eventos -> lista de eventos
-- Salida:  lista de Strings con el resumen
-- ============================================================
promedioPorCategoriaAnio :: [Evento] -> [String]
promedioPorCategoriaAnio eventos =
    concatMap resumenAnio anios
  where
    -- Obtiene el año de cada evento
    anioEvento e  = let (a,_,_,_) = timestampAFecha (timestamp e) in a
    anios         = nubSimple (map anioEvento eventos)
    -- Genera el resumen para un año específico
    resumenAnio a =
        let delAnio = filter (\e -> anioEvento e == a) eventos
            cats    = nubSimple (map categoria delAnio)
        in  ("--- Año " ++ show a ++ " ---") : map (resumirCat delAnio a) cats
    -- Calcula promedio de una categoría en un año dado
    resumirCat evs a cat =
        let delCat = filter (\e -> categoria e == cat) evs
            prom   = sum (map valor delCat) / fromIntegral (length delCat)
        in  "  " ++ cat ++ ": promedio = " ++ show prom
            ++ " (" ++ show (length delCat) ++ " eventos en " ++ show a ++ ")"

-- ============================================================
-- ANÁLISIS TEMPORAL: mesMayorMonto
-- Encuentra el mes con mayor monto total acumulado.
-- Entrada: eventos -> lista de eventos
-- Salida:  String con el resultado
-- ============================================================
mesMayorMonto :: [Evento] -> String
mesMayorMonto [] = "No hay eventos."
mesMayorMonto eventos =
    let -- Extrae el mes de cada evento
        mesEvento e   = let (_,m,_,_) = timestampAFecha (timestamp e) in m
        meses         = [1..12]
        -- Suma montos por mes
        totalMes m    = sum (map valor (filter (\e -> mesEvento e == m) eventos))
        montosPorMes  = map (\m -> (m, totalMes m)) meses
        (mejorMes, t) = maximumBy (comparing snd) montosPorMes
    in  "Mes con mayor monto: " ++ nombreMes mejorMes ++ " | Total: " ++ show t

-- ============================================================
-- ANÁLISIS TEMPORAL: diaMasActivo
-- Encuentra el día de la semana con más eventos.
-- Entrada: eventos -> lista de eventos
-- Salida:  String con el resultado
-- ============================================================
diaMasActivo :: [Evento] -> String
diaMasActivo [] = "No hay eventos."
diaMasActivo eventos =
    let -- Obtiene el día de la semana de cada evento
        diaEvento e      = let (_,_,_,d) = timestampAFecha (timestamp e) in d
        dias             = [0..6]
        -- Cuenta eventos por día
        cantDia d        = length (filter (\e -> diaEvento e == d) eventos)
        cantsPorDia      = map (\d -> (d, cantDia d)) dias
        (mejorDia, cant) = maximumBy (comparing snd) cantsPorDia
    in  "Día más activo: " ++ nombreDia mejorDia ++ " | Eventos: " ++ show cant

-- ============================================================
-- BÚSQUEDA: buscarPorRango
-- Filtra eventos cuyo timestamp está entre dos fechas dadas.
-- Entrada: tsInicio, tsFin -> timestamps límite, eventos -> lista
-- Salida:  lista de eventos dentro del rango
-- ============================================================
buscarPorRango :: Int -> Int -> [Evento] -> [Evento]
buscarPorRango tsInicio tsFin eventos =
    filter (\e -> timestamp e >= tsInicio && timestamp e <= tsFin) eventos

-- ============================================================
-- ESTADÍSTICAS: cantidadPorCategoria
-- Cuenta cuántos eventos hay por cada categoría.
-- Entrada: eventos -> lista de eventos
-- Salida:  lista de Strings con el conteo
-- ============================================================
cantidadPorCategoria :: [Evento] -> [String]
cantidadPorCategoria eventos =
    map resumen categorias
  where
    resumen cat =
        let cantidad = length (filter (\e -> categoria e == cat) eventos)
        in  "  " ++ cat ++ ": " ++ show cantidad ++ " evento(s)"

-- ============================================================
-- ESTADÍSTICAS: eventoMaxMin
-- Encuentra el evento con monto más alto y más bajo.
-- Entrada: eventos -> lista de eventos
-- Salida:  String con ambos resultados
-- ============================================================
eventoMaxMin :: [Evento] -> String
eventoMaxMin [] = "No hay eventos."
eventoMaxMin eventos =
    let validos = filter validarMonto eventos
        eMax    = maximumBy (comparing valor) validos
        eMin    = maximumBy (comparing (Down . valor)) validos
    in  "  Máximo -> ID:" ++ show (eventoId eMax)
        ++ " | Cat:" ++ categoria eMax
        ++ " | Valor:" ++ show (valor eMax)
        ++ "\n  Mínimo -> ID:" ++ show (eventoId eMin)
        ++ " | Cat:" ++ categoria eMin
        ++ " | Valor:" ++ show (valor eMin)

-- ============================================================
-- EXPORTACIÓN: exportarCSV
-- Convierte la lista de eventos a formato CSV.
-- Entrada: eventos -> lista de eventos
-- Salida:  String con el contenido CSV
-- ============================================================
exportarCSV :: [Evento] -> String
exportarCSV eventos =
    let encabezado = "id,categoria,valor,timestamp"
        filas      = map eventoACSV eventos
    in  intercalate "\n" (encabezado : filas)
  where
    eventoACSV e = intercalate ","
        [ show (eventoId e)
        , categoria e
        , show (valor e)
        , show (timestamp e)
        ]

-- ============================================================
-- EXPORTACIÓN: exportarJSON
-- Convierte la lista de eventos a formato JSON.
-- Entrada: eventos -> lista de eventos
-- Salida:  String con el contenido JSON
-- ============================================================
exportarJSON :: [Evento] -> String
exportarJSON eventos =
    "[\n" ++ intercalate ",\n" (map eventoAJSON eventos) ++ "\n]"
  where
    eventoAJSON e = "  { \"id\": "        ++ show (eventoId e)
                 ++ ", \"categoria\": \"" ++ categoria e  ++ "\""
                 ++ ", \"valor\": "       ++ show (valor e)
                 ++ ", \"timestamp\": "   ++ show (timestamp e) ++ " }"

-- ============================================================
-- UTILIDAD: nubSimple
-- Elimina duplicados de una lista (reemplaza Data.List.nub).
-- Entrada: lista con posibles duplicados
-- Salida:  lista sin duplicados
-- ============================================================
nubSimple :: Eq a => [a] -> [a]
nubSimple []     = []
nubSimple (x:xs) = x : nubSimple (filter (/= x) xs)

-- ============================================================
-- MENÚ: mostrarMenu
-- Imprime las opciones disponibles del sistema.
-- ============================================================
mostrarMenu :: IO ()
mostrarMenu = do
    putStrLn ""
    putStrLn "========================================"
    putStrLn "|   Sistema de Eventos Comerciales     |"
    putStrLn "|======================================|"
    putStrLn "|  1. Transformación de eventos        |"
    putStrLn "|  2. Análisis de datos                |"
    putStrLn "|  3. Análisis temporal                |"
    putStrLn "|  4. Búsqueda por rango de fechas     |"
    putStrLn "|  5. Estadísticas                     |"
    putStrLn "|  6. Salir                            |"
    putStrLn "|======================================|"
    putStr "Seleccione una opción: "

-- ============================================================
-- MENÚ: menuTransformacion
-- Submenú para las opciones de transformación.
-- Entrada: eventos -> lista actual, gen -> generador
-- Salida:  IO ([Evento], StdGen) -> lista actualizada y generador
-- ============================================================
menuTransformacion :: [Evento] -> StdGen -> IO ([Evento], StdGen)
menuTransformacion eventos gen = do
    putStrLn ""
    putStrLn "-- Transformación de eventos --"
    putStrLn "  1. Etiquetar eventos de alto valor"
    putStrLn "  2. Volver"
    putStr "Opción: "
    opcion <- getLine
    case opcion of
        "1" -> do
            putStrLn ""
            putStrLn "Eventos etiquetados:"
            let etiquetados = etiquetarAltoValor (filter validarMonto eventos)
            mapM_ putStrLn etiquetados
            reportarInconsistencias eventos
            let (nuevos, gNuevo) = alimentarSistema eventos gen
            putStrLn $ "\n[Sistema] Se agregaron nuevos eventos. Total: " ++ show (length nuevos)
            return (nuevos, gNuevo)
        _ -> return (eventos, gen)

-- ============================================================
-- MENÚ: menuAnalisisDatos
-- Submenú para las opciones de análisis de datos.
-- Entrada: eventos -> lista actual, gen -> generador
-- Salida:  IO ([Evento], StdGen)
-- ============================================================
menuAnalisisDatos :: [Evento] -> StdGen -> IO ([Evento], StdGen)
menuAnalisisDatos eventos gen = do
    putStrLn ""
    putStrLn "-- Análisis de datos --"
    putStrLn "  1. Promedio de monto por categoría por año"
    putStrLn "  2. Volver"
    putStr "Opción: "
    opcion <- getLine
    case opcion of
        "1" -> do
            putStrLn ""
            putStrLn "Promedio por categoría y año:"
            let resumen = promedioPorCategoriaAnio (filter validarMonto eventos)
            mapM_ putStrLn resumen
            reportarInconsistencias eventos
            let (nuevos, gNuevo) = alimentarSistema eventos gen
            putStrLn $ "\n[Sistema] Se agregaron nuevos eventos. Total: " ++ show (length nuevos)
            return (nuevos, gNuevo)
        _ -> return (eventos, gen)

-- ============================================================
-- MENÚ: menuAnalisisTemporal
-- Submenú para las opciones de análisis temporal.
-- Entrada: eventos -> lista actual, gen -> generador
-- Salida:  IO ([Evento], StdGen)
-- ============================================================
menuAnalisisTemporal :: [Evento] -> StdGen -> IO ([Evento], StdGen)
menuAnalisisTemporal eventos gen = do
    putStrLn ""
    putStrLn "-- Análisis temporal --"
    putStrLn "  1. Mes con mayor monto y día más activo"
    putStrLn "  2. Volver"
    putStr "Opción: "
    opcion <- getLine
    case opcion of
        "1" -> do
            putStrLn ""
            putStrLn (mesMayorMonto eventos)
            putStrLn (diaMasActivo eventos)
            reportarInconsistencias eventos
            let (nuevos, gNuevo) = alimentarSistema eventos gen
            putStrLn $ "\n[Sistema] Se agregaron nuevos eventos. Total: " ++ show (length nuevos)
            return (nuevos, gNuevo)
        _ -> return (eventos, gen)

-- ============================================================
-- MENÚ: menuBusqueda
-- Permite buscar eventos por rango de timestamps.
-- Entrada: eventos -> lista actual, gen -> generador
-- Salida:  IO ([Evento], StdGen)
-- ============================================================
menuBusqueda :: [Evento] -> StdGen -> IO ([Evento], StdGen)
menuBusqueda eventos gen = do
    putStrLn ""
    putStrLn "-- Búsqueda por rango de fechas --"
    putStrLn "Ingrese timestamp inicial (ej: 1746000000):"
    putStr "> "
    tsInicioStr <- getLine
    putStrLn "Ingrese timestamp final (ej: 1777000000):"
    putStr "> "
    tsFinStr <- getLine
    let tsInicio  = read tsInicioStr :: Int
        tsFin     = read tsFinStr    :: Int
        resultado = buscarPorRango tsInicio tsFin eventos
    putStrLn ""
    if null resultado
        then putStrLn "No se encontraron eventos en ese rango."
        else do
            putStrLn $ "Eventos encontrados: " ++ show (length resultado)
            mapM_ print resultado
    reportarInconsistencias eventos
    let (nuevos, gNuevo) = alimentarSistema eventos gen
    putStrLn $ "\n[Sistema] Se agregaron nuevos eventos. Total: " ++ show (length nuevos)
    return (nuevos, gNuevo)

-- ============================================================
-- MENÚ: menuEstadisticas
-- Muestra estadísticas y permite exportar en CSV o JSON.
-- Entrada: eventos -> lista actual, gen -> generador
-- Salida:  IO ([Evento], StdGen)
-- ============================================================
menuEstadisticas :: [Evento] -> StdGen -> IO ([Evento], StdGen)
menuEstadisticas eventos gen = do
    putStrLn ""
    putStrLn "-- Estadísticas --"
    putStrLn ""
    putStrLn "A. Resumen general:"
    putStrLn "Cantidad por categoría:"
    mapM_ putStrLn (cantidadPorCategoria eventos)
    putStrLn ""
    putStrLn "Evento con monto más alto y más bajo:"
    putStrLn (eventoMaxMin eventos)
    putStrLn ""
    putStrLn "¿Desea exportar? (csv / json / no):"
    putStr "> "
    resp <- getLine
    case map toLower resp of
        "csv" -> do
            let contenido  = exportarCSV eventos
                nombreArchivo = "estadisticas.csv"
            writeFile nombreArchivo contenido
            putStrLn $ "Exportado como: " ++ nombreArchivo
        "json" -> do
            let contenido  = exportarJSON eventos
                nombreArchivo = "estadisticas.json"
            writeFile nombreArchivo contenido
            putStrLn $ "Exportado como: " ++ nombreArchivo
        _ -> putStrLn "Sin exportación."
    reportarInconsistencias eventos
    let (nuevos, gNuevo) = alimentarSistema eventos gen
    putStrLn $ "\n[Sistema] Se agregaron nuevos eventos. Total: " ++ show (length nuevos)
    return (nuevos, gNuevo)

-- ============================================================
-- BUCLE PRINCIPAL: bucleMenu
-- Mantiene el menú activo hasta que el usuario elija salir.
-- Entrada: eventos -> lista actual, gen -> generador
-- Salida:  IO () termina el programa al salir
-- ============================================================
bucleMenu :: [Evento] -> StdGen -> IO ()
bucleMenu eventos gen = do
    mostrarMenu
    opcion <- getLine
    case opcion of
        "1" -> do
            (nuevos, gNuevo) <- menuTransformacion eventos gen
            bucleMenu nuevos gNuevo
        "2" -> do
            (nuevos, gNuevo) <- menuAnalisisDatos eventos gen
            bucleMenu nuevos gNuevo
        "3" -> do
            (nuevos, gNuevo) <- menuAnalisisTemporal eventos gen
            bucleMenu nuevos gNuevo
        "4" -> do
            (nuevos, gNuevo) <- menuBusqueda eventos gen
            bucleMenu nuevos gNuevo
        "5" -> do
            (nuevos, gNuevo) <- menuEstadisticas eventos gen
            bucleMenu nuevos gNuevo
        "6" -> putStrLn "\n¡Hasta luego! Cerrando el sistema..."
        _   -> do
            putStrLn "Opción no válida. Intente de nuevo."
            bucleMenu eventos gen

-- ============================================================
-- FUNCIÓN PRINCIPAL: main
-- Inicializa el sistema generando eventos iniciales
-- y lanza el menú interactivo.
-- ============================================================
main :: IO ()
main = do
    putStrLn "Iniciando Sistema de Eventos Comerciales..."
    gen <- newStdGen
    let (eventosIniciales, g1) = generarEventos 15 gen
    putStrLn $ "Se cargaron " ++ show (length eventosIniciales) ++ " eventos iniciales."
    bucleMenu eventosIniciales g1
