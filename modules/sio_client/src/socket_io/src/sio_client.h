//
//  sio_client.h
//
//  Created by Melo Yao on 3/25/15.
//

#ifndef SIO_CLIENT_H
#define SIO_CLIENT_H
#include <string>
#include <functional>
#include "sio_message.h"
#include "sio_socket.h"

namespace sio
{
    class client_impl;
    
    class client {
    public:
        enum close_reason
        {
            close_reason_normal,
            close_reason_drop
        };
        
        using con_listener = std::function<void ()>;
        
        using close_listener = std::function<void (const close_reason &)>;

        using reconnect_listener = std::function<void (unsigned int, unsigned int)>;
        
        using socket_listener = std::function<void (const std::string &)>;
        
        client();
        ~client();
        
        //set listeners and event bindings.
        void set_open_listener(con_listener const& l);
        
        void set_fail_listener(con_listener const& l);
        
        void set_reconnecting_listener(con_listener const& l);

        void set_reconnect_listener(reconnect_listener const& l);

        void set_close_listener(close_listener const& l);
        
        void set_socket_open_listener(socket_listener const& l);
        
        void set_socket_close_listener(socket_listener const& l);
        
        void clear_con_listeners();
        
        void clear_socket_listeners();
        
        // Client Functions - such as send, etc.
        void connect(const std::string& uri);

        void connect(const std::string& uri, const std::map<std::string,std::string>& query);

        void connect(const std::string& uri, const std::map<std::string,std::string>& query,
                     const std::map<std::string,std::string>& http_extra_headers);

        void set_reconnect_attempts(int attempts);

        void set_reconnect_delay(unsigned millis);

        void set_reconnect_delay_max(unsigned millis);
        
        sio::socket::ptr const& socket(const std::string& nsp = "");
        
        // Closes the connection
        void close();
        
        void sync_close();
        
        bool opened() const;
        
        std::string const& get_sessionid() const;
        
    private:
        //disable copy constructor and assign operator.
        client(client const& /*unused*/){}
        void operator=(client const& /*unused*/){}
        
        client_impl* m_impl;
    };
    
}  // namespace sio


#endif // __SIO_CLIENT__H__
