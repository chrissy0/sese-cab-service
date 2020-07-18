# Responsibilities

## Chanki Hong

**Rolle:** Quality Manager

**erledigte Aufgaben:** 

- Aufbau des Cabs inclusiv von Sensoren
- Initialisierung der Sensoren von Cab
- Implementierung der Algorithmen in Webots 
  (Line Detection, To the Depot, Obstacle Avoidance)
- Verfassung des Coding standards
  ( C , Socket Programming , ADA , JAVA )
- Umsetzung die Algorithmen von Webots ins Ada
  ( Line Detection, Roadmarker )
- Unit testing in External Controller
  ( Line detection, Roadmarker, Motor Controller )
- Verification durch SPARK
  ( Roadmarker , Motor Controller , Front Distance )

## Christopher Woggon

**Rolle:** Technical Manager (zu Anfang: Quality Manager)

**erledigte Aufgaben:** 

- Webots
    - Line following in C++ angepasst
    - Collision detection in C++ entwickelt und umgesetzt, Sensoren an Cab angebracht
    - Neue Welt von Grund auf erstellt (Layout, Boden, Linien, Curbs, Road Marker, Depot)
    - Cab mit Sensoren (+ Backup-Sensoren) ausgestattet (für collision detection, curb detection, line following, road marker detection, wall detection)
    - Motoren an Cab angebracht um Curb Detection Sensoren hoch- und runterzufahren (+ Konzept entwickelt)
    - Road Marker Konzept entwickelt, Auswertungsalgorithmus is C++ implementiert
- Quality Management
    - QA Konzept erstellt (`QA.md`)
- Technical Management
    - Server
        - 2 VPS
        - Installation, Konfiguration
        - Regelmäßiges Deployment von Frontend + Backend auf beiden Servern
    - Jenkins
        - Installation und Einrichtung
        - docker-compose file um Ada Code in Pipeline zu kompilieren
        - Java Tests
    - git
        - Umzug des Repos
        - Pull Request Bedingungen
            - Jenkins Pipeline
            - Review
- Backend entwickelt
    - Sehr umfangreich getestet
    - REST Schnittstellen + Logik für
        - Jobs
        - Routes
        - Location
        - Registration
        - Pickup
        - Dropoff
        - Blocked
        - Dysfunctional
        - Debug
        - Reset
    - Logik für
        - PathFinder (Routenfindung zwischen Sektionen)
    - Schnittstellenkonzepte größtenteils mit Maximilian Weisenseel entwickelt (`interface-definitions.md`)
- Frontend entwickelt
- Anleitung für Installation/Ausführung von Backend/Frontend erstellt

        

## Florentin Ehser

**Rolle:** Time Manager

**erledigte Aufgaben:** 

- Zeitplan aufgestellt und aktuell gehalten
- Arbeitspakete gemeinsam mit Gruppe erstellt und verteilt
- Projektziele & Anforderungen gesammelt und festgelegt
- Risiken und mögliche Strategien gesammelt
- strukturierte Anforderungen für Teilsysteme erstellt
- Sicherheitsebenen definiert
- grundsätzliche Funktionsweise des Gesamtsystems konzeptioniert und weiterentwickelt
- Statechart von Gesamtsystem entwickelt
- Erstes Konzept für Backend erstellt
- Erste Unit-Tests für Statechart geschrieben
- verbesserte Anordnung der Roadmarker entwickelt
- Lanefollowing-Konzept überarbeitet
- Anpassungen in der Webots-Simulation:
- Konzept verschieden farbiger Mittellinien umgesetzt
- Roadmarker angepasst
- Designverbesserung Cab
- zweites Cab
- Unit-Tests für externen Controller kontrolliert und korrigiert
- Backend pair-reviewed
- System-Tests durchgeführt
- System mit Anforderungen abgeglichen
- Anleitung Simulation geschrieben

- Dokumentation der Sensoren geschrieben

## Julian Hartmer

**Rolle:** Integrationsbeauftragter

**erledigte Aufgaben:** 

- Festlegen der Schnittstelle Externer Controller <-> Webots (mit Max)
- Implementierung des Ringbuffers für die Kommunikation
- Entwurf Struktur Externer Kontroller auf Grundlage des Statecharts
- Definition der internen Schnittstellen des externen Controllers
- Implementierung der Pakete Motor Controller, Externer Controller, Lane Detection, Roadmarker (mit Chanki) und Front Distance
- Testen der oben genannten Pakete (mit Florentin und Chanki)
- Skript zur Ausführung der ADA Unit Tests in CI
- Skeleton zur Erstellung der Unittests in ADA

## Maximilian Weissenseel

**Rolle:** 

**erledigte Aufgaben:** 

- STICHPUNKTE
