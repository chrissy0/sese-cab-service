package de.tuberlin.sese.cabservice.persistence.cab.registration;

import de.tuberlin.sese.cabservice.persistence.cab.location.CabLocationEntity;
import de.tuberlin.sese.cabservice.persistence.cab.location.CabLocationService;
import de.tuberlin.sese.cabservice.persistence.cab.registration.persistence.CabEntity;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.junit4.SpringRunner;

import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;

@RunWith(SpringRunner.class)
@SpringBootTest
public class CabRegistrationServiceIT {

    @Autowired
    private CabRegistrationService registrationService;

    @Autowired
    private CabLocationService locationService;

    @Test
    public void shouldSaveCabLocationUponRegistration() {

        long cabId = registrationService.registerCab(CabEntity.builder()
                .name("Some Cab Name")
                .build(), 4);

        List<CabLocationEntity> locations = locationService.getAllCabLocations();
        assertThat(locations).hasSize(1);
        CabLocationEntity location = locations.get(0);
        assertThat(location.getCabId()).isEqualTo(cabId);
        assertThat(location.getSection()).isEqualTo(4);
    }
}
