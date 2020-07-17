package de.tuberlin.sese.cabservice.persistence.cab.registration;

import de.tuberlin.sese.cabservice.persistence.cab.location.CabLocationEntity;
import de.tuberlin.sese.cabservice.persistence.cab.location.CabLocationService;
import de.tuberlin.sese.cabservice.persistence.cab.registration.persistence.CabEntity;
import de.tuberlin.sese.cabservice.util.exceptions.NameAlreadyInUseException;
import de.tuberlin.sese.cabservice.util.exceptions.UnknownSectionException;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.test.context.junit4.SpringRunner;

import static java.util.Collections.singletonList;
import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;

@RunWith(SpringRunner.class)
@SpringBootTest
public class CabRegistrationServiceTest {

    @Autowired
    private CabRegistrationService registrationService;

    @MockBean
    private CabLocationService locationService;

    @MockBean
    private CabRepo repo;

    @Captor
    private ArgumentCaptor<CabEntity> cabEntityCaptor;

    @Captor
    private ArgumentCaptor<CabLocationEntity> locationEntityCaptor;

    @Test
    public void shouldRegisterCab() {
        when(repo.save(any(CabEntity.class))).thenReturn(CabEntity.builder()
                .id(3L)
                .name("Some Cab Name")
                .build());

        long cabId = registrationService.registerCab(CabEntity.builder()
                .name("Some Cab Name")
                .build(), 5);

        verify(repo).findAll();
        verify(repo).save(cabEntityCaptor.capture());
        verify(locationService).saveCabLocation(locationEntityCaptor.capture());
        verifyNoMoreInteractions(repo);
        verifyNoMoreInteractions(locationService);

        assertThat(cabId).isEqualTo(3);

        CabEntity capturedCabEntity = cabEntityCaptor.getValue();
        assertThat(capturedCabEntity.getId()).isNull();
        assertThat(capturedCabEntity.getName()).isEqualTo("Some Cab Name");

        CabLocationEntity capturedLocationEntity = locationEntityCaptor.getValue();
        assertThat(capturedLocationEntity.getCabId()).isEqualTo(3);
        assertThat(capturedLocationEntity.getSection()).isEqualTo(5);
    }

    @Test
    public void shouldThrowIllegalArgumentExceptionOnMissingCabEntity() {
        assertThatThrownBy(() -> registrationService.registerCab(null, 5))
                .isExactlyInstanceOf(IllegalArgumentException.class)
                .hasMessage("CabEntity was null");
    }

    @Test
    public void shouldThrowIllegalArgumentExceptionOnMissingSection() {
        assertThatThrownBy(() -> registrationService.registerCab(CabEntity.builder()
                .name("Some Cab Name")
                .build(), null))
                .isExactlyInstanceOf(IllegalArgumentException.class)
                .hasMessage("Section was null");
    }

    @Test
    public void shouldThrowIllegalArgumentExceptionOnExistingCabId() {
        assertThatThrownBy(() -> registrationService.registerCab(CabEntity.builder()
                .id(3L)
                .name("Some Cab Name")
                .build(), 4))
                .isExactlyInstanceOf(IllegalArgumentException.class)
                .hasMessage("Id of CabEntity was not null");
    }

    @Test
    public void shouldThrowIllegalArgumentExceptionOnMissingName() {
        assertThatThrownBy(() -> registrationService.registerCab(CabEntity.builder()
                .build(), 4))
                .isExactlyInstanceOf(IllegalArgumentException.class)
                .hasMessage("Name of CabEntity was null or empty");
    }

    @Test
    public void shouldThrowIllegalArgumentExceptionOnEmptyName() {
        assertThatThrownBy(() -> registrationService.registerCab(CabEntity.builder()
                .name("")
                .build(), 4))
                .isExactlyInstanceOf(IllegalArgumentException.class)
                .hasMessage("Name of CabEntity was null or empty");
    }

    @Test
    public void shouldThrowNameAlreadyInUseExceptionIfNameAlreadyInUse() {
        when(repo.findAll()).thenReturn(singletonList(CabEntity.builder()
                .id(0L)
                .name("Some Name")
                .build()));

        assertThatThrownBy(() -> registrationService.registerCab(CabEntity.builder()
                .name("Some Name")
                .build(), 4))
                .isExactlyInstanceOf(NameAlreadyInUseException.class)
                .hasMessage("CabName \"Some Name\" is already in use");
    }

    @Test
    public void shouldThrowUnknownSectionExceptionIfSectionIsUnknown() {
        assertThatThrownBy(() -> registrationService.registerCab(CabEntity.builder()
                .name("Some Name")
                .build(), 16))
                .isExactlyInstanceOf(UnknownSectionException.class)
                .hasMessage("Section \"16\" is unknown");
    }
}
